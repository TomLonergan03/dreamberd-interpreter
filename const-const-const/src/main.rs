use actix_web::{get, post, web, App, HttpResponse, HttpServer, Responder};
use serde::Deserialize;

mod get_set_vars;
pub use get_set_vars::{kv_get_var, kv_set_var};

#[get("/")]
async fn hello() -> impl Responder {
    HttpResponse::Ok().body("Hello world!")
}

#[get("/get/{var}")]
async fn get_var(path: web::Path<(String,)>) -> impl Responder {
    let key = &path.into_inner().0;
    let value = kv_get_var(key).unwrap();
    if value.is_none() {
        return HttpResponse::NotFound().body(format!("const const const {} does not exist!", key));
    }
    HttpResponse::Ok().body(value.unwrap())
}

#[derive(Deserialize)]
struct PostData {
    key: String,
    value: String,
}

#[post("/set")]
async fn set_var(data: web::Json<PostData>) -> impl Responder {
    let existing_value = kv_get_var(&data.key).unwrap();
    if existing_value.is_some() {
        return HttpResponse::BadRequest().body(format!(
            "const const const {} already exists! (value: {})",
            &data.key,
            existing_value.unwrap()
        ));
    }
    kv_set_var(&data.key, &data.value).unwrap();
    HttpResponse::Ok().body(format!(
        "const const const {} set to {}",
        &data.key, &data.value
    ))
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    HttpServer::new(|| App::new().service(hello).service(get_var).service(set_var))
        .bind(("127.0.0.1", 8080))?
        .run()
        .await
}
