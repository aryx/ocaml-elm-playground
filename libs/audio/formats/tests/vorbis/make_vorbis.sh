#!/bin/sh
# claude: how the Vorbis files here were made, once (sh make_vorbis.sh,
# from this directory, with ffmpeg 6 and its libvorbis): the sounds of
# ../mpeg/make_mpeg.sh (our bell, a second of it; and the bell on the
# left with chirps on the right, which have attacks: short blocks),
# encoded by someone else's encoders, and what ffmpeg decodes from
# each, for the tests to compare ours with.
#   bell.ogg      mono, libvorbis, quality 3 (residue 1)
#   stereo.ogg    libvorbis, quality 4: the two channels coupled
#                 (residue 2), long and short blocks
#   low.ogg       libvorbis, 22,050 Hz, quality 0: its smaller blocks
#   native.ogg    ffmpeg's own encoder (stereo only), whose setup is
#                 not libvorbis's: other codebooks, floors and modes
set -e
root=$(cd ../../../../.. && pwd)
(cd "$root" && dune build ./apps/media/tests/Dump_media.exe)
tmp=$(mktemp -d)
"$root/_build/default/apps/media/tests/Dump_media.exe" "$tmp" > /dev/null
ff="ffmpeg -v error -y"
$ff -i "$tmp/bell.wav" -t 1 "$tmp/bell1.wav"
$ff -i "$tmp/bell1.wav" \
  -f lavfi -i "aevalsrc=0.5*exp(-30*mod(t\,0.25))*sin(2*PI*(300+3000*mod(t\,0.25))*t):s=44100:d=1" \
  -filter_complex "[0:a]asplit[l][b];[1:a]volume=0.3[c];[b][c]amix=inputs=2:normalize=0[r];[l][r]amerge=inputs=2[s]" \
  -map "[s]" -ac 2 "$tmp/stereo.wav"
plain="-map_metadata -1 -fflags +bitexact -flags:a +bitexact"
$ff -i "$tmp/bell1.wav" -c:a libvorbis -q:a 3 $plain bell.ogg
$ff -i "$tmp/stereo.wav" -c:a libvorbis -q:a 4 $plain stereo.ogg
$ff -i "$tmp/stereo.wav" -ar 22050 -c:a libvorbis -q:a 0 $plain low.ogg
$ff -i "$tmp/stereo.wav" -c:a vorbis -strict experimental -b:a 96k $plain native.ogg
# decoded by libvorbis, the reference (ffmpeg's own decoder gives another
# right channel for native.ogg)
for f in bell stereo low native; do
  $ff -c:a libvorbis -i $f.ogg -c:a pcm_s16le $plain $f.expected.wav
done
rm -r "$tmp"
