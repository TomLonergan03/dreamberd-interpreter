use dotenv::dotenv;
use redis::Commands;

fn get_redis_connection() -> redis::Connection {
    dotenv().ok();
    let redis_url = std::env::var("REDIS_URL").expect("REDIS_URL must be set");
    let client = redis::Client::open(redis_url).unwrap();
    let con = client.get_connection().unwrap();
    con
}

pub fn kv_get_var(key: &str) -> redis::RedisResult<Option<String>> {
    let mut con = get_redis_connection();
    con.get(key)
}

pub fn kv_set_var(key: &str, value: &str) -> redis::RedisResult<()> {
    let mut con = get_redis_connection();
    con.set(key, value)
}
