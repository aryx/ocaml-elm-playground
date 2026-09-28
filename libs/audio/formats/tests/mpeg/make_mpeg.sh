#!/bin/sh
# claude: how the MPEG audio files here were made, once (sh make_mpeg.sh,
# from this directory, with ffmpeg 6, its libtwolame and libmp3lame):
# our own bell (Our_media's bell.wav, dumped by
# apps/media/tests/Dump_media.ml), a second of it, encoded by
# someone else's encoders -- until we have our own -- and what ffmpeg
# decodes from each, for the tests to compare ours with. The stereo
# files have the bell on the left, and on the right the bell and
# chirps, four a second: two channels alike but not the same (so the
# encoders choose joint stereo: mid/side in MP3, in 39 frames of 40)
# and attacks (short blocks, in MP3).
#   bell.mp2      mono, ffmpeg's MP2 encoder, 128 kbit/s
#   stereo.mp2    twolame, joint stereo, 64 kbit/s (32 a channel: 8
#                 bands, Table B.2c; every frame joint stereo)
#   bell.mp3      mono, LAME, 64 kbit/s
#   stereo.mp3    LAME, joint stereo, 128 kbit/s
#   lsf.mp2       MPEG-2's lower rates: twolame, 24,000 Hz, joint
#                 stereo, 64 kbit/s (ISO/IEC 13818-3's allocation table)
#   lsf_mono.mp3  MPEG-2's lower rates: LAME, 22,050 Hz, 32 kbit/s
#   lsf_stereo.mp3              and 24,000 Hz, joint stereo, 64 kbit/s
# No Xing/LAME tag (-write_xing 0): ffmpeg would trim the encoder's
# delay it records, and our decoder doesn't read it.
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
$ff -i "$tmp/bell1.wav" -c:a mp2 -b:a 128k bell.mp2
$ff -i "$tmp/stereo.wav" -c:a libtwolame -mode joint_stereo -b:a 64k stereo.mp2
$ff -i "$tmp/bell1.wav" -c:a libmp3lame -b:a 64k -write_xing 0 bell.mp3
$ff -i "$tmp/stereo.wav" -c:a libmp3lame -b:a 128k -joint_stereo 1 -write_xing 0 stereo.mp3
$ff -i "$tmp/stereo.wav" -ar 24000 -c:a libtwolame -mode joint_stereo -b:a 64k lsf.mp2
$ff -i "$tmp/bell1.wav" -ar 22050 -c:a libmp3lame -b:a 32k -write_xing 0 lsf_mono.mp3
$ff -i "$tmp/stereo.wav" -ar 24000 -c:a libmp3lame -b:a 64k -joint_stereo 1 -write_xing 0 lsf_stereo.mp3
for f in bell.mp2 stereo.mp2 lsf.mp2 bell.mp3 stereo.mp3 lsf_mono.mp3 lsf_stereo.mp3; do
  $ff -i $f -c:a pcm_s16le "${f%.*}_${f#*.}.expected.wav"
done
rm -r "$tmp"
