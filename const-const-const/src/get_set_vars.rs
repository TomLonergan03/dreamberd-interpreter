use redis::Commands;

const REDIS_URL: &str =
    "rediss://default:49690c6635524252939fd901166ee01c@unified-cicada-37801.upstash.io:37801";

fn get_redis_connection() -> redis::Connection {
    let client = redis::Client::open(REDIS_URL).unwrap();
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
