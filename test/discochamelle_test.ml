let () =
  let x = Discochamelle.Rest.Guilds.query_param_list_to_query_string [`Limit 32; `After "asd"] in
  let r = "?limit=32&after=asd&" in
  assert (x = r)
