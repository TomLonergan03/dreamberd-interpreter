#include <boost/asio.hpp>
#include <boost/beast.hpp>
#include <iostream>

#include "json.hpp"

using tcp = boost::asio::ip::tcp;
namespace http = boost::beast::http;
using json = nlohmann::json;

const std::string kAddition = "+";
const std::string kSubtraction = "-";
const std::string kMultiplication = "*";
const std::string kDivision = "/";

struct evaluation {
  int value1;
  int value2;
  std::string operation;
  int result;
};

evaluation fromJSON(json& j) {
  evaluation e;
  j.at("value1").get_to(e.value1);
  j.at("value2").get_to(e.value2);
  j.at("operation").get_to(e.operation);
  return e;
}

json toJSON(evaluation& e) { return json{{"result", e.result}}; }

void computeResult(evaluation& e) {
  try {
    if (e.operation == kAddition) {
      e.result = e.value1 + e.value2;
    } else if (e.operation == kSubtraction) {
      e.result = e.value1 - e.value2;
    } else if (e.operation == kMultiplication) {
      e.result = e.value1 * e.value2;
    } else if (e.operation == kDivision) {
      e.result = e.value1 / e.value2;
    } else {
      std::cerr << "Operation " << e.operation << " not supported" << std::endl;
    }
  } catch (...) {
    std::cerr << "Error computing result" << std::endl;
    e.result = 0;
  }
}

void handleRequest(http::request<http::string_body>& request,
                   tcp::socket& socket) {
  json data = request.body();
  std::cout << data << std::endl;

  evaluation e = fromJSON(data);
  computeResult(e);

  // Prepare the response message
  http::response<http::string_body> response;
  response.version(request.version());
  response.result(http::status::ok);
  response.set(http::field::server, "DreamBerd Mathematics");
  response.set(http::field::content_type, "text/plain");
  response.body() = toJSON(e);
  std::cout << response.body() << std::endl;
  response.prepare_payload();

  // Send the response to the client
  boost::beast::http::write(socket, response);
}

void runServer() {
  boost::asio::io_context io_context;
  tcp::acceptor acceptor(io_context, {tcp::v4(), 8080});

  while (true) {
    tcp::socket socket(io_context);
    acceptor.accept(socket);

    // Read the HTTP request
    boost::beast::flat_buffer buffer;
    http::request<http::string_body> request;
    boost::beast::http::read(socket, buffer, request);

    // Handle the request
    handleRequest(request, socket);

    // Close the socket
    socket.shutdown(tcp::socket::shutdown_send);
  }
}

int main() {
  try {
    runServer();
  } catch (const std::exception& e) {
    std::cerr << "Exception: " << e.what() << std::endl;
  }

  return 0;
}