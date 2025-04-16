open Lwt
open Spotify_interfaces

module Com_canonical_dbusmenu = struct
  open Com_canonical_dbusmenu

  let ( let* ) = Lwt.bind

  let get_layout proxy ~parentId ~recursionDepth ~propertyNames =
    let parentId = Int32.of_int parentId in
    let recursionDepth = Int32.of_int recursionDepth in
    let* revision, layout =
      OBus_method.call m_GetLayout proxy
        (parentId, recursionDepth, propertyNames)
    in
    let revision = Int32.to_int revision in
    let layout = (fun (x1, x2, x3) -> (Int32.to_int x1, x2, x3)) layout in
    return (revision, layout)

  let get_group_properties proxy ~ids ~propertyNames =
    let ids = List.map Int32.of_int ids in
    let* properties =
      OBus_method.call m_GetGroupProperties proxy (ids, propertyNames)
    in
    let properties =
      List.map (fun (x1, x2) -> (Int32.to_int x1, x2)) properties
    in
    return properties

  let get_property proxy ~id ~name =
    let id = Int32.of_int id in
    OBus_method.call m_GetProperty proxy (id, name)

  let event proxy ~id ~eventId ~data ~timestamp =
    let id = Int32.of_int id in
    let timestamp = Int32.of_int timestamp in
    OBus_method.call m_Event proxy (id, eventId, data, timestamp)

  let event_group proxy ~events =
    let events =
      List.map
        (fun (x1, x2, x3, x4) -> (Int32.of_int x1, x2, x3, Int32.of_int x4))
        events
    in
    let* idErrors = OBus_method.call m_EventGroup proxy events in
    let idErrors = List.map Int32.to_int idErrors in
    return idErrors

  let about_to_show proxy ~id =
    let id = Int32.of_int id in
    OBus_method.call m_AboutToShow proxy id

  let about_to_show_group proxy ~ids =
    let ids = List.map Int32.of_int ids in
    let* updatesNeeded, idErrors =
      OBus_method.call m_AboutToShowGroup proxy ids
    in
    let updatesNeeded = List.map Int32.to_int updatesNeeded in
    let idErrors = List.map Int32.to_int idErrors in
    return (updatesNeeded, idErrors)

  let items_properties_updated proxy =
    OBus_signal.map
      (fun (updatedProps, removedProps) ->
        let updatedProps =
          List.map (fun (x1, x2) -> (Int32.to_int x1, x2)) updatedProps
        in
        let removedProps =
          List.map (fun (x1, x2) -> (Int32.to_int x1, x2)) removedProps
        in
        (updatedProps, removedProps))
      (OBus_signal.make s_ItemsPropertiesUpdated proxy)

  let layout_updated proxy =
    OBus_signal.map
      (fun (revision, parent) ->
        let revision = Int32.to_int revision in
        let parent = Int32.to_int parent in
        (revision, parent))
      (OBus_signal.make s_LayoutUpdated proxy)

  let item_activation_requested proxy =
    OBus_signal.map
      (fun (id, timestamp) ->
        let id = Int32.to_int id in
        let timestamp = Int32.to_int timestamp in
        (id, timestamp))
      (OBus_signal.make s_ItemActivationRequested proxy)

  let version proxy =
    OBus_property.map_r
      (fun x -> Int32.to_int x)
      (OBus_property.make p_Version proxy)

  let text_direction proxy = OBus_property.make p_TextDirection proxy
  let status proxy = OBus_property.make p_Status proxy
  let icon_theme_path proxy = OBus_property.make p_IconThemePath proxy
end

module Org_kde_StatusNotifierItem = struct
  open Org_kde_StatusNotifierItem

  let scroll proxy ~delta ~orientation =
    let delta = Int32.of_int delta in
    OBus_method.call m_Scroll proxy (delta, orientation)

  let secondary_activate proxy ~x ~y =
    let x = Int32.of_int x in
    let y = Int32.of_int y in
    OBus_method.call m_SecondaryActivate proxy (x, y)

  let xayatana_secondary_activate proxy ~timestamp =
    let timestamp = Int32.of_int timestamp in
    OBus_method.call m_XAyatanaSecondaryActivate proxy timestamp

  let new_icon proxy = OBus_signal.make s_NewIcon proxy
  let new_icon_theme_path proxy = OBus_signal.make s_NewIconThemePath proxy
  let new_attention_icon proxy = OBus_signal.make s_NewAttentionIcon proxy
  let new_status proxy = OBus_signal.make s_NewStatus proxy
  let xayatana_new_label proxy = OBus_signal.make s_XAyatanaNewLabel proxy
  let new_title proxy = OBus_signal.make s_NewTitle proxy
  let id proxy = OBus_property.make p_Id proxy
  let category proxy = OBus_property.make p_Category proxy
  let status proxy = OBus_property.make p_Status proxy
  let icon_name proxy = OBus_property.make p_IconName proxy
  let icon_accessible_desc proxy = OBus_property.make p_IconAccessibleDesc proxy
  let attention_icon_name proxy = OBus_property.make p_AttentionIconName proxy

  let attention_accessible_desc proxy =
    OBus_property.make p_AttentionAccessibleDesc proxy

  let title proxy = OBus_property.make p_Title proxy
  let icon_theme_path proxy = OBus_property.make p_IconThemePath proxy

  let menu proxy =
    OBus_property.map_r_with_context
      (fun context x ->
        (fun x -> OBus_proxy.make ~peer:(OBus_context.sender context) ~path:x) x)
      (OBus_property.make p_Menu proxy)

  let xayatana_label proxy = OBus_property.make p_XAyatanaLabel proxy
  let xayatana_label_guide proxy = OBus_property.make p_XAyatanaLabelGuide proxy

  let xayatana_ordering_index proxy =
    OBus_property.map_r
      (fun x -> Int32.to_int x)
      (OBus_property.make p_XAyatanaOrderingIndex proxy)
end

module Org_mpris_MediaPlayer2 = struct
  open Org_mpris_MediaPlayer2

  let raise proxy = OBus_method.call m_Raise proxy ()
  let quit proxy = OBus_method.call m_Quit proxy ()
  let can_quit proxy = OBus_property.make p_CanQuit proxy
  let can_set_fullscreen proxy = OBus_property.make p_CanSetFullscreen proxy
  let can_raise proxy = OBus_property.make p_CanRaise proxy
  let has_track_list proxy = OBus_property.make p_HasTrackList proxy
  let identity proxy = OBus_property.make p_Identity proxy
  let desktop_entry proxy = OBus_property.make p_DesktopEntry proxy

  let supported_uri_schemes proxy =
    OBus_property.make p_SupportedUriSchemes proxy

  let supported_mime_types proxy = OBus_property.make p_SupportedMimeTypes proxy
end

module Org_mpris_MediaPlayer2_Player = struct
  open Org_mpris_MediaPlayer2_Player

  let next proxy = OBus_method.call m_Next proxy ()
  let previous proxy = OBus_method.call m_Previous proxy ()
  let pause proxy = OBus_method.call m_Pause proxy ()
  let play_pause proxy = OBus_method.call m_PlayPause proxy ()
  let stop proxy = OBus_method.call m_Stop proxy ()
  let play proxy = OBus_method.call m_Play proxy ()
  let seek proxy ~offset = OBus_method.call m_Seek proxy offset

  let set_position proxy ~trackid ~position =
    let trackid = OBus_proxy.path trackid in
    OBus_method.call m_SetPosition proxy (trackid, position)

  let open_uri proxy ~uri = OBus_method.call m_OpenUri proxy uri
  let seeked proxy = OBus_signal.make s_Seeked proxy
  let playback_status proxy = OBus_property.make p_PlaybackStatus proxy
  let loop_status proxy = OBus_property.make p_LoopStatus proxy
  let rate proxy = OBus_property.make p_Rate proxy
  let shuffle proxy = OBus_property.make p_Shuffle proxy
  let metadata proxy = OBus_property.make p_Metadata proxy
  let volume proxy = OBus_property.make p_Volume proxy
  let position proxy = OBus_property.make p_Position proxy
  let minimum_rate proxy = OBus_property.make p_MinimumRate proxy
  let maximum_rate proxy = OBus_property.make p_MaximumRate proxy
  let can_go_next proxy = OBus_property.make p_CanGoNext proxy
  let can_go_previous proxy = OBus_property.make p_CanGoPrevious proxy
  let can_play proxy = OBus_property.make p_CanPlay proxy
  let can_pause proxy = OBus_property.make p_CanPause proxy
  let can_seek proxy = OBus_property.make p_CanSeek proxy
  let can_control proxy = OBus_property.make p_CanControl proxy
end
