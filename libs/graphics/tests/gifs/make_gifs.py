# claude: how Unit_gif's fixtures were made (python3 make_gifs.py, with PIL),
# except interlaced.gif, made by ImageMagick:
#   convert -size 16x21 gradient:red-blue -colors 16 -interlace GIF interlaced.gif

import random, struct
from PIL import Image

def u16(n): return struct.pack('<H', n)

def lzw_uncompressed(indices, mcs):
    # a valid LZW stream without compression: a clear before the
    # dictionary would make the width grow, so every code has mcs+1 bits
    clear, end = 1 << mcs, (1 << mcs) + 1
    width = mcs + 1
    codes = [clear]
    since = 0
    for i in indices:
        # the decoder adds an entry per code after the first: stay below 2^width
        if since == (1 << width) - (end + 1) - 1:
            codes.append(clear); since = 0
        codes.append(i); since += 1
    codes.append(end)
    acc = n = 0
    for c in codes: acc |= c << n; n += width
    data = acc.to_bytes((n + 7) // 8, 'little')
    blocks = b''.join(bytes([len(data[k:k+255])]) + data[k:k+255] for k in range(0, len(data), 255))
    return bytes([mcs]) + blocks + b'\x00'

def palette(colors, bits):
    colors = colors + [(0, 0, 0)] * ((1 << bits) - len(colors))
    return b''.join(bytes(c) for c in colors)

# 1. the worked example: 6x1, 4 colors, data 02 | 02 8C 5F | 00
pal4 = [(0, 0, 0), (255, 0, 0), (0, 255, 0), (0, 0, 255)]
open('lzw.gif', 'wb').write(b'GIF89a' + u16(6) + u16(1) + bytes([0x81, 0, 0]) + palette(pal4, 2)
    + b'\x2C' + u16(0) + u16(0) + u16(6) + u16(1) + b'\x00' + b'\x02\x02\x8C\x5F\x00' + b'\x3B')

# 2. 256 random colors, 64x64: the dictionary fills, clears
random.seed(42)
im = Image.new('P', (64, 64))
im.putpalette([random.randrange(256) for _ in range(768)])
im.putdata([random.randrange(256) for _ in range(64 * 64)])
im.save('noise.gif')

# 4. an animation, 8x8: a full first frame, then patches
def frame(x, y, w, h, indices, disposal, delay, transparent=None, local=None, bits=2):
    gce = b'\x21\xF9\x04' + bytes([(disposal << 2) | (1 if transparent is not None else 0)]) + u16(delay) \
        + bytes([transparent or 0]) + b'\x00'
    flags = (0x80 | (bits - 1)) if local else 0
    return gce + b'\x2C' + u16(x) + u16(y) + u16(w) + u16(h) + bytes([flags]) \
        + (palette(local, bits) if local else b'') + lzw_uncompressed(indices, max(2, bits))
anim = b'GIF89a' + u16(8) + u16(8) + bytes([0x81, 0, 0]) + palette(pal4, 2)
anim += b'\x21\xFF\x0BNETSCAPE2.0\x03\x01\x00\x00\x00'
anim += frame(0, 0, 8, 8, [(x + y) % 4 for y in range(8) for x in range(8)], 1, 10)
anim += frame(2, 2, 3, 3, [0, 1, 0, 1, 2, 1, 0, 1, 0], 3, 0, transparent=0,
              local=[(9, 9, 9), (200, 100, 50), (50, 100, 200), (1, 2, 3)])
anim += frame(4, 5, 4, 2, [3, 3, 2, 2, 1, 1, 0, 0], 2, 1)
anim += frame(0, 0, 2, 2, [1, 2, 3, 1], 1, 25, transparent=2)
anim += b'\x3B'
open('anim.gif', 'wb').write(anim)
