use simple_xml_parser::{match_literal,identifier,right,Parser};

fn main() {
    let sample = "<parent attr=\"value\"><child1></child1></parent>";

    let node_parser = right(match_literal("<"), identifier);
    let result = node_parser.parse(sample).unwrap().1;

    println!("node name: {:?}", result);
}
