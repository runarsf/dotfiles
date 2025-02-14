_: {
  mkSecretFile = {
    source,
    destination,
    ...
  }: {
    sopsFile = source;
    path = destination;
    format = "binary";
  };
}
