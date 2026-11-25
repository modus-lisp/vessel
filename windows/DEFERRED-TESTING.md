# Windows host: testing deferred

What the Windows host (`src/win-host.lisp`, `src/win-ui-*.lisp`, `windows/`) does that has **not** been checked yet,
and why. Each item says what was checked instead and how to check the rest. Tick an item (`[x]`) and say how when it
has been verified.

Most items wait on hardware this development PC lacks (a camera, a real microphone, a second Bluetooth device, an HDR
display mode), on a person (typing, picking in a dialog, looking at the screen), or on a change to Windows' settings.

Commands run from the workspace root. `modus` below is `.\modus-win64-hosted.exe`. Widget tests use the bench
`src/win-ui-harness.lisp` (see its header).

## Needs hardware

- [ ] **Camera** (`cam_*`, `win-host.lisp` "the camera"): written on a PC with no camera. Checked: the code loads,
  "no camera" is handled, and the capture thread's machine code was checked by disassembly. Not run: frames, the
  preview, a photo (JPEG through WIC), a recording (H.264 MP4, no sound), and Windows' camera privacy refusal.
  On a PC with a camera: `(kiln-camera:camera-names)`, open one, read `camera-frame` a few times, take a photo,
  record 5 s and play the file.
- [ ] **Microphone with a real voice** (`mic_*`): checked at 48,000 samples/s with none dropped, but only from
  silent virtual inputs. Check the level moves with speech and a recording plays back.
- [ ] **Dictation from the microphone** (`ear_*`, SAPI): the file path is verified (a WAV transcribed word for word);
  live listening starts but has only heard silence. Speak a sentence and read `ear_text`.
- [ ] **Bluetooth LE link** (`ble_*`): the radio comes on, scans and offers the service. Not done: a link to a peer
  and data both ways. Needs a second device running chirp: run `modus --script vessel\src\chirp-windows.lisp`
  beside a phone or Mac running chirp, and check each sees the other in Nearby and their messages cross.
- [ ] **Spatial sound** (`spatial_*`, ISpatialAudioClient): only the fallback (panning) has run; this PC's output has
  no spatial format on. Turn on Windows Sonic for Headphones (Settings > System > Sound > the device > Spatial
  audio), run hello3d, and listen for the hums placed in 3D as the camera turns.
- [ ] **HDR on an HDR display** (`gpu_view_hdr`, `gpu_headroom`): the half-float scRGB path runs in hello3d without
  errors, with Windows' HDR off (headroom 1.0). Turn Windows' HDR on (Settings > Display > HDR) and check: the
  headroom and SDR white the log prints are plausible, the middle cube glows brighter than white, and ordinary
  white matches the desktop's.

## Needs a person at the screen

- [ ] **Toasts** (`notify_post`): seen on screen (2026-11-24), once the app's Start menu shortcut carried its ID
  (`ModusLisp.Vessel.<title>`, also registered under `HKCU\Software\Classes\AppUserModelId`). Not yet checked:
  that a click on one brings the app forward, and the badge icon (the exe's own, not the Vessel logo).
- [ ] **hello3d looks the same in HDR mode on an SDR display.** Its view now always draws through the HDR path; it
  should look exactly as before. Nobody has looked yet.
- [ ] **Typing** (WM_CHAR text input): real typing of letters, spaces, `;` and Backspace is verified. Not typed yet:
  Shift symbols (`!@#`), a dead key (`´` then `e`), the emoji panel (Win+.), and an IME (Win+Space). Also: the
  IME's composition window is not placed at a text cursor.
- [ ] **Clipboard copy** (`app_copy`): processes started from the agent sandbox are refused the clipboard (PowerShell's
  `Get-Clipboard` too), so it has never been seen to work. From your own terminal, run a script holding
  `(load "vessel/src/win-ui-harness.lisp") (print (vessel-windows::clipboard-set "héllo from modus ✓"))`;
  expect `1`, then paste: expect "héllo from modus ✓". The same function backs `ui_share_present`.
- [ ] **Dark title bar**: this PC is in light mode. Switch Windows to dark (Settings > Personalization > Colors >
  "Choose your app mode") with a vessel window open: its title bar should turn dark at once.
- [ ] **Media keys** (now playing): every command is verified through Windows' session API, but not by pressing a
  keyboard's media keys or a headset button. Also untested: a cover given as image bytes (`np_artwork_data`, written
  to `%TEMP%` first).
- [ ] **Widget dialogs and menus**, each implemented with its result handler tested directly, but never driven by a
  person:
  - [ ] colour well and `ui_color_picker_show`: pick a colour, expect `(:color h #xRRGGBB)`
  - [ ] `ui_font_picker_show`: pick a font, expect `(:font h "Name")`
  - [ ] `ui_image_picker_show`: the Pictures filter, then pick a picture
  - [ ] context, edit and row-action menus (right-click or long press): choose an item
  - [ ] calendar: click a day (the real MCN_SELECT notification)
  - [ ] stepper: keyboard arrows
- [ ] **chirp by hand** (`win-ui-app.lisp`, the native app layer): pictured by its tour (`CHIRP_TOUR=dir`: chat,
  a message sent, Nearby, Radio Log, Nickname), where Send and Back were called directly because clicks posted by a
  script were handled several frames late. Click Send and Back with the mouse, press Enter in the compose bar,
  choose from the ⋯ menu, and Save a new nickname.
- [ ] **Widgets by eye**: the placeholder text in text fields and search bars, the spinner's animation, the icon
  glyphs `ui_image_new` maps (about 20 SF Symbol names to Segoe Fluent Icons code points, from memory), page dots,
  and the coloured panels and shapes.

## Needs a change to Windows' settings

- [ ] **MSIX identity** (`windows/build-identity.ps1`): the unsigned package builds (the manifest passes makeappx) and
  `vessel.exe` runs with its embedded identity manifest. Not done: creating a signing certificate, trusting it
  (elevated), installing the signed package, launching "Vessel" from Start, and checking the process has the
  package identity. The commands are in the script's header.

## Known gaps (not tests: things not built yet)

- `ui_fade_to` under plain `modus.exe` only shows or hides: Windows refuses WS_EX_LAYERED on a child window to an
  exe whose manifest declares no Windows 8+ compatibility (measured: refused for modus.exe, allowed for the same
  exe with a Windows 10 `supportedOS` manifest). `vessel.exe` (build-identity.ps1) now declares it, and fades for
  real there; `modus.exe` itself would need its build to embed such a manifest.
- Lists: the Delete key and F5 are not wired (a list's keyboard messages do not reach Lisp without subclassing);
  delete and refresh are in the row's right-click menu.
- Grids and collections do not scroll; moving a stack's child does not re-lay out the stack.
- `ui_blur_new` is a dark fill, not a blur. `ui_share_present` copies to the clipboard (no share sheet).
- Snapshots and printing do not work on an HDR view (they read 8-bit back buffers).
- Camera recordings have no sound.
- The native app layer (`app_*`) covers what chirp uses: navigation, bar buttons and menus, tables, the compose bar,
  forms, Documents, the UTC offset. Not yet: browsers, screens shown whole (`app_screen`/`app_show`), alerts,
  status bar, previews, sharing, table appends and placeholders, and the rest of Native.swift. Table rows are one
  line ("title — subtitle"); the compose bar has no placeholder.
- Head tracking, device motion and the watch link answer "not here" (no PC equivalent).

## Already verified (for reference)

Toasts (seen on screen), window, frame pacing (flip model), raw-input mouse look, WASAPI speaker, generational GC, microphone capture rate,
speech (aloud, to a file, file transcription), now playing (all commands through the session API), file and folder
dialogs (picked by a person), Bluetooth radio start, text input (real typing), and the ~80 widgets' 210 automated
checks (all five files loaded together).
