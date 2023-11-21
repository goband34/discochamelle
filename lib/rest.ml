open Cohttp
open Cohttp_lwt_unix
open Ppx_yojson_conv_lib.Yojson_conv

let (let*) = Lwt.bind

let make_request endpoint =
  let config = Config.read_config () in
  let url = Printf.sprintf "%s%s" config.base_url endpoint in
  let headers = Header.of_list [
    ("Authorization", Printf.sprintf "Bot %s" config.token);
    ("User-Agent", "DiscordBot (https://discord.com/api/v10, 10)")
  ] in
  let* _, body = Client.get ~headers (Uri.of_string url) in
  Lwt.return body

type snowflake = string [@@deriving yojson]

module Role = struct
  module RoleTags = struct
    type t = {
      bot_id: snowflake option;
      integration_id: snowflake option;
      premium_subscriber: bool;
      subscription_listing_id: snowflake option;
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
                | _ -> {record with bot_id = Some (snowflake_of_yojson value)}
            end
            | "integration_id" -> begin
                match value with
                | `Null -> {record with integration_id = None}
                | _ -> {record with integration_id = Some (snowflake_of_yojson value)}
            end
            | "premium_subscriber" -> begin
                match value with 
                | `Null -> {record with premium_subscriber = true}
                | _ -> raise (Failure "role_tag: premium_subscriber expected null or missing")
            end
            | "subscription_listing_id" -> begin
                match value with
                | `Null -> {record with subscription_listing_id = None}
                | _ -> {record with subscription_listing_id = Some (snowflake_of_yojson value)}
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
          | Some x -> Some (k, yojson_of_snowflake x)
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
    id: snowflake;
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
end

(* placeholders so the LSP stops nagging me *)
type emoji = int [@@deriving yojson]
type guild_feature = int [@@deriving yojson]
type welcome_screen = int [@@deriving yojson]
type sticker = int [@@deriving yojson]

(* plumbing APIs is a pain *)
type guild = {
  id: snowflake;
  name: string;
  icon: string option;
  icon_hash: string option [@yojson.option];
  splash: string option;
  discovery_splash: string option;
  owner: bool option [@yojson.option];
  owner_id: snowflake;
  permissions: string option  [@yojson.option];
  afk_channel_id: snowflake option;
  afk_timeout: int;
  widget_enabled: bool option [@yojson.option];
  widget_channel_id: snowflake option [@yojson.option];
  verification_level: int;
  default_message_notifications: int;
  explicit_content_filter: int;
  roles: Role.t array;
  emojis:	emoji array;
  features:	guild_feature array;
  mfa_level: int;
  application_id: snowflake option;
  system_channel_id: snowflake option;
  system_channel_flags: int;
  rules_channel_id: snowflake option;
  max_presences: int option [@yojson.option];
  max_members: int option [@yojson.option];
  vanity_url_code: string option;
  description: string option;
  banner: string option;
  premium_tier: int;
  premium_subscription_count: int option [@yojson.option];
  preferred_locale: string;
  public_updates_channel_id: snowflake option;
  max_video_channel_users: int option [@yojson.option];
  max_stage_video_channel_users: int option [@yojson.option];
  approximate_member_count: int option [@yojson.option];
  approximate_presence_count: int option [@yojson.option];
  welcome_screen: welcome_screen option [@yojson.option];
  nsfw_level:	int;
  stickers: sticker array option [@yojson.option];
  premium_progress_bar_enabled:	bool;
  safety_alerts_channel_id:	snowflake option;
} [@@deriving yojson]

(* TODO: parse body into guild array when the types are done *)
let my_guilds () =
  let* b = make_request "users/@me/guilds" in
  b |> Cohttp_lwt.Body.to_string
