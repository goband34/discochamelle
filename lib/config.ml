let base_url () =
  "https://dcdcf503-2c17-4a09-ac05-7d49cf81a7f9.mock.pstmn.io/https://discord.com/api/v10/"

let url_for_endpoint s =
  let b = base_url () in
  b ^ s
