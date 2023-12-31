open Ppx_yojson_conv_lib.Yojson_conv

module RoleTags = struct
  type t = {
      bot_id: Snowflake.t option;
      integration_id: Snowflake.t option;
      premium_subscriber: bool;
      subscription_listing_id: Snowflake.t option;
      available_for_purchase: bool;
      guild_connections: bool;
    }

  let default = {
      bot_id = None;
      integration_id = None;
      premium_subscriber = false;
      subscription_listing_id = None;
      available_for_purchase = false;
      guild_connections = false;
    }

  let t_of_yojson = function
    | `Assoc fields ->
       let replace_in_record record (field,value) =
         match field with
         | "bot_id" -> begin
             match value with
             | `Null -> {record with bot_id = None}
             | _ -> {record with bot_id = Some (Snowflake.t_of_yojson value)}
           end
         | "integration_id" -> begin
             match value with
             | `Null -> {record with integration_id = None}
             | _ -> {record with integration_id = Some (Snowflake.t_of_yojson value)}
           end
         | "premium_subscriber" -> begin
             match value with 
             | `Null -> {record with premium_subscriber = true}
             | _ -> raise (Failure "role_tag: premium_subscriber expected null or missing")
           end
         | "subscription_listing_id" -> begin
             match value with
             | `Null -> {record with subscription_listing_id = None}
             | _ -> {record with subscription_listing_id = Some (Snowflake.t_of_yojson value)}
           end
         | "available_for_purchase" -> begin
             match value with
             | `Null -> {record with available_for_purchase = true}
             | _ -> raise (Failure "role_tag: available_for_purchase expected null or missing")
           end
         | "guild_connections" -> begin
             match value with 
             | `Null -> {record with guild_connections = true}
             | _ -> raise (Failure "role_tag: guild_connections expected null or missing")
           end
         | k -> raise (Failure (Printf.sprintf "role_tag invalid key %s" k))
       in 
       List.fold_left replace_in_record default fields
    | _ -> raise (Failure "Role tags invalid structure")

  let yojson_of_t x =
    let pairs = 
      [("bot_id", `Snowflake x.bot_id);
       ("integration_id", `Snowflake x.integration_id);
       ("premium_subscriber", `NullBool x.premium_subscriber);
       ("subscription_listing_id", `Snowflake x.subscription_listing_id);
       ("available_for_purchase", `NullBool x.available_for_purchase);
       ("guild_connections", `NullBool x.guild_connections)]
    in
    pairs |> List.filter_map (fun (k, v) -> 
                 match v with
                 | `Snowflake x -> begin
                     match x with
                     | None -> None
                     | Some x -> Some (k, Snowflake.yojson_of_t x)
                   end
                 | `NullBool x -> begin
                     match x with
                     | false -> None
                     | true -> Some (k, `Null)
                   end
               ) |> fun pairs ->
                    `Assoc pairs
end

type t = {
    id: Snowflake.t;
    name: string;
    color: int;
    hoist: bool;
    icon: string option [@yojson.option];
    unicode_emoji: string option [@yojson.option];
    position: int;
    permissions: string;
    managed: bool;
    mentionable: bool;
    tags: RoleTags.t option [@yojson.option];
    flags: int;
} [@@deriving yojson]
