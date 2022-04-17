use syn::parse::ParseStream;
use syn::punctuated::Punctuated;
use syn::{Attribute, FnArg, Token};

pub fn parse_fn_args(input: ParseStream) -> Result<Punctuated<FnArg, Token![,]>, syn::Error> {
    let mut args = Punctuated::new();
    let mut has_receiver = false;

    while !input.is_empty() {
        let attrs = input.call(Attribute::parse_outer)?;

        let mut arg: FnArg = input.parse()?;
        match &mut arg {
            FnArg::Receiver(receiver) if has_receiver => {
                return Err(syn::Error::new(
                    receiver.self_token.span,
                    "unexpected second method receiver",
                ));
            }
            FnArg::Receiver(receiver) if !args.is_empty() => {
                return Err(syn::Error::new(
                    receiver.self_token.span,
                    "unexpected method receiver",
                ));
            }
            FnArg::Receiver(receiver) => {
                has_receiver = true;
                receiver.attrs = attrs;
            }
            FnArg::Typed(arg) => arg.attrs = attrs,
        }
        args.push_value(arg);

        if input.is_empty() {
            break;
        }

        let comma: Token![,] = input.parse()?;
        args.push_punct(comma);
    }

    Ok(args)
}
