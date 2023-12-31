open Ppx_yojson_conv_lib.Yojson_conv

type emoji = {
    id: int;
    name: string option;
    roles: int array option [@yojson.option]; (* array of role ids *)
    user: User.t option [@yojson.option];
    require_colons: bool [@default false];
    managed: bool [@default false];
    animated: bool [@default false];
    available: bool [@default false];
} [@@deriving yojson]

type guild_feature =
  | ANIMATED_BANNER	
  | ANIMATED_ICON	
  | APPLICATION_COMMAND_PERMISSIONS_V2	
  | AUTO_MODERATION	
  | BANNER	
  | COMMUNITY	
  | CREATOR_MONETIZABLE_PROVISIONAL	
  | CREATOR_STORE_PAGE	
  | DEVELOPER_SUPPORT_SERVER	
  | DISCOVERABLE	
  | FEATURABLE	
  | INVITES_DISABLED	
  | INVITE_SPLASH	
  | MEMBER_VERIFICATION_GATE_ENABLED	
  | MORE_STICKERS	
  | NEWS	
  | PARTNERED	
  | PREVIEW_ENABLED	
  | RAID_ALERTS_DISABLED	
  | ROLE_ICONS	
  | ROLE_SUBSCRIPTIONS_AVAILABLE_FOR_PURCHASE	
  | ROLE_SUBSCRIPTIONS_ENABLED	
  | TICKETED_EVENTS_ENABLED	
  | VANITY_URL	
  | VERIFIED	
  | VIP_REGIONS	
  | WELCOME_SCREEN_ENABLED	[@@deriving yojson]

type welcome_screen_channel = {
    channel_id: Snowflake.t;
    description: string;
    emoji_id: Snowflake.t option;
    emoji_name: string option;
} [@@deriving yojson]

type welcome_screen = {
    description: string option;
    welcome_channels: welcome_screen_channel array;
} [@@deriving yojson]

type sticker = {
    id: Snowflake.t;
    pack_id: Snowflake.t option [@yojson.option];
    name: string;
    description: string option;
    tags: string;
    asset: string option [@yojson.option]; (* deprecated *)
    sticker_type: int [@key "type"];
    format_type: int;
    available: bool option [@yojson.option];
    guild_id: Snowflake.t option [@yojson.option];
    user: User.t option [@yojson.option];
    sort_value: int option [@yojson.option];
} [@@deriving yojson]

(* plumbing APIs is a pain *)
type t = {
  id: Snowflake.t;
  name: string;
  icon: string option;
  icon_hash: string option [@yojson.option];
  splash: string option;
  discovery_splash: string option;
  owner: bool option [@yojson.option];
  owner_id: Snowflake.t;
  permissions: string option  [@yojson.option];
  afk_channel_id: Snowflake.t option;
  afk_timeout: int;
  widget_enabled: bool option [@yojson.option];
  widget_channel_id: Snowflake.t option [@yojson.option];
  verification_level: int;
  default_message_notifications: int;
  explicit_content_filter: int;
  roles: Role.t array;
  emojis: emoji array;
  features: guild_feature array;
  mfa_level: int;
  application_id: Snowflake.t option;
  system_channel_id: Snowflake.t option;
  system_channel_flags: int;
  rules_channel_id: Snowflake.t option;
  max_presences: int option [@yojson.option];
  max_members: int option [@yojson.option];
  vanity_url_code: string option;
  description: string option;
  banner: string option;
  premium_tier: int;
  premium_subscription_count: int option [@yojson.option];
  preferred_locale: string;
  public_updates_channel_id: Snowflake.t option;
  max_video_channel_users: int option [@yojson.option];
  max_stage_video_channel_users: int option [@yojson.option];
  approximate_member_count: int option [@yojson.option];
  approximate_presence_count: int option [@yojson.option];
  welcome_screen: welcome_screen option [@yojson.option];
  nsfw_level:	int;
  stickers: sticker array option [@yojson.option];
  premium_progress_bar_enabled:	bool;
  safety_alerts_channel_id:	Snowflake.t option;
} [@@deriving yojson]
