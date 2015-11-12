pub fn read_file(filename: &str) -> Result<String, String> {
    use std::fs::File;
    use std::io::Read;

    let mut file = match File::open(filename) {
        Err(why) => {
            return Err(format!("couldn't open {}: {}", filename, why));
        }
        Ok(file) => file,
    };
    let mut code = String::new();
    match file.read_to_string(&mut code) {
        Err(why) => {
            return Err(format!("couldn't read {}: {}", filename, why));
        }
        Ok(_) => { }
    }
    return Ok(code);
}
