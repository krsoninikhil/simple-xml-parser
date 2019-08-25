pub fn match_literal(expected: &'static str) -> impl Fn(&str) -> Result<(&str, ()), &str> {
    move |input| match input.get(0..expected.len()) {
        Some(next) if next == expected => Ok((&input[expected.len()..], ())),
        _ => Err(input),
    }
}

pub fn identifier(input: &str) -> Result<(&str, String), &str> {
    let mut matched = String::new();
    let mut chars = input.chars();

    match chars.next() {
        Some(next) if next.is_alphabetic() => matched.push(next),
        _ => return Err(input),
    }

    while let Some(next) = chars.next() {
        if next.is_alphanumeric() || next == '-' {
            matched.push(next)
        } else {
            break
        }
    }

    return Ok((&input[matched.len()..], matched))
}

#[test]
fn literal_parser() {
    let parse_div = match_literal("<div>");

    assert_eq!(
        Ok(("", ())),
        parse_div("<div>")
    );
    assert_eq!(
        Ok(("</div>", ())),
        parse_div("<div></div>")
    );
    assert_eq!(
        Err("<div id=\"test\"></div>"),
        parse_div("<div id=\"test\"></div>")
    );
}

#[test]
fn identifier_parser() {
    assert_eq!(
        Ok(("", "an-identifier".to_string())),
        identifier("an-identifier")
    );
    assert_eq!(
        Ok((" and more", "an-identifier".to_string())),
        identifier("an-identifier and more")
    );
    assert_eq!(
        Err("!not-an-identifier"),
        identifier("!not-an-identifier")
    );
}
