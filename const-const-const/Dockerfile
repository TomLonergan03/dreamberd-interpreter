FROM rust:latest
COPY ./ ./
RUN cargo build --release
EXPOSE 8080

ENTRYPOINT ["./target/release/const-const-const"]
