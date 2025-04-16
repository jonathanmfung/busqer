module Com_canonical_dbusmenu : sig
  val get_layout :
    OBus_proxy.t ->
    parentId:int ->
    recursionDepth:int ->
    propertyNames:string list ->
    (int
    * (int * (string * OBus_value.V.single) list * OBus_value.V.single list))
    Lwt.t

  val get_group_properties :
    OBus_proxy.t ->
    ids:int list ->
    propertyNames:string list ->
    (int * (string * OBus_value.V.single) list) list Lwt.t

  val get_property :
    OBus_proxy.t -> id:int -> name:string -> OBus_value.V.single Lwt.t

  val event :
    OBus_proxy.t ->
    id:int ->
    eventId:string ->
    data:OBus_value.V.single ->
    timestamp:int ->
    unit Lwt.t

  val event_group :
    OBus_proxy.t ->
    events:(int * string * OBus_value.V.single * int) list ->
    int list Lwt.t

  val about_to_show : OBus_proxy.t -> id:int -> bool Lwt.t

  val about_to_show_group :
    OBus_proxy.t -> ids:int list -> (int list * int list) Lwt.t

  val items_properties_updated :
    OBus_proxy.t ->
    ((int * (string * OBus_value.V.single) list) list
    * (int * string list) list)
    OBus_signal.t

  val layout_updated : OBus_proxy.t -> (int * int) OBus_signal.t
  val item_activation_requested : OBus_proxy.t -> (int * int) OBus_signal.t
  val version : OBus_proxy.t -> (int, [ `readable ]) OBus_property.t
  val text_direction : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t
  val status : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t

  val icon_theme_path :
    OBus_proxy.t -> (string list, [ `readable ]) OBus_property.t
end

module Org_kde_StatusNotifierItem : sig
  val scroll : OBus_proxy.t -> delta:int -> orientation:string -> unit Lwt.t
  val secondary_activate : OBus_proxy.t -> x:int -> y:int -> unit Lwt.t
  val xayatana_secondary_activate : OBus_proxy.t -> timestamp:int -> unit Lwt.t
  val new_icon : OBus_proxy.t -> unit OBus_signal.t
  val new_icon_theme_path : OBus_proxy.t -> string OBus_signal.t
  val new_attention_icon : OBus_proxy.t -> unit OBus_signal.t
  val new_status : OBus_proxy.t -> string OBus_signal.t
  val xayatana_new_label : OBus_proxy.t -> (string * string) OBus_signal.t
  val new_title : OBus_proxy.t -> unit OBus_signal.t
  val id : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t
  val category : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t
  val status : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t
  val icon_name : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t

  val icon_accessible_desc :
    OBus_proxy.t -> (string, [ `readable ]) OBus_property.t

  val attention_icon_name :
    OBus_proxy.t -> (string, [ `readable ]) OBus_property.t

  val attention_accessible_desc :
    OBus_proxy.t -> (string, [ `readable ]) OBus_property.t

  val title : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t
  val icon_theme_path : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t
  val menu : OBus_proxy.t -> (OBus_proxy.t, [ `readable ]) OBus_property.t
  val xayatana_label : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t

  val xayatana_label_guide :
    OBus_proxy.t -> (string, [ `readable ]) OBus_property.t

  val xayatana_ordering_index :
    OBus_proxy.t -> (int, [ `readable ]) OBus_property.t
end

module Org_mpris_MediaPlayer2 : sig
  val raise : OBus_proxy.t -> unit Lwt.t
  val quit : OBus_proxy.t -> unit Lwt.t
  val can_quit : OBus_proxy.t -> (bool, [ `readable ]) OBus_property.t
  val can_set_fullscreen : OBus_proxy.t -> (bool, [ `readable ]) OBus_property.t
  val can_raise : OBus_proxy.t -> (bool, [ `readable ]) OBus_property.t
  val has_track_list : OBus_proxy.t -> (bool, [ `readable ]) OBus_property.t
  val identity : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t
  val desktop_entry : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t

  val supported_uri_schemes :
    OBus_proxy.t -> (string list, [ `readable ]) OBus_property.t

  val supported_mime_types :
    OBus_proxy.t -> (string list, [ `readable ]) OBus_property.t
end

module Org_mpris_MediaPlayer2_Player : sig
  val next : OBus_proxy.t -> unit Lwt.t
  val previous : OBus_proxy.t -> unit Lwt.t
  val pause : OBus_proxy.t -> unit Lwt.t
  val play_pause : OBus_proxy.t -> unit Lwt.t
  val stop : OBus_proxy.t -> unit Lwt.t
  val play : OBus_proxy.t -> unit Lwt.t
  val seek : OBus_proxy.t -> offset:int64 -> unit Lwt.t

  val set_position :
    OBus_proxy.t -> trackid:OBus_proxy.t -> position:int64 -> unit Lwt.t

  val open_uri : OBus_proxy.t -> uri:string -> unit Lwt.t
  val seeked : OBus_proxy.t -> int64 OBus_signal.t
  val playback_status : OBus_proxy.t -> (string, [ `readable ]) OBus_property.t

  val loop_status :
    OBus_proxy.t -> (string, [ `readable | `writable ]) OBus_property.t

  val rate : OBus_proxy.t -> (float, [ `readable | `writable ]) OBus_property.t

  val shuffle :
    OBus_proxy.t -> (bool, [ `readable | `writable ]) OBus_property.t

  val metadata :
    OBus_proxy.t ->
    ((string * OBus_value.V.single) list, [ `readable ]) OBus_property.t

  val volume :
    OBus_proxy.t -> (float, [ `readable | `writable ]) OBus_property.t

  val position : OBus_proxy.t -> (int64, [ `readable ]) OBus_property.t
  val minimum_rate : OBus_proxy.t -> (float, [ `readable ]) OBus_property.t
  val maximum_rate : OBus_proxy.t -> (float, [ `readable ]) OBus_property.t
  val can_go_next : OBus_proxy.t -> (bool, [ `readable ]) OBus_property.t
  val can_go_previous : OBus_proxy.t -> (bool, [ `readable ]) OBus_property.t
  val can_play : OBus_proxy.t -> (bool, [ `readable ]) OBus_property.t
  val can_pause : OBus_proxy.t -> (bool, [ `readable ]) OBus_property.t
  val can_seek : OBus_proxy.t -> (bool, [ `readable ]) OBus_property.t
  val can_control : OBus_proxy.t -> (bool, [ `readable ]) OBus_property.t
end
