# claude: the picture of the Image* examples (ImageJpeg, ImagePng,
# ImageLzw), in the three formats they take apart: python3
# make_demo_pictures.py, with PIL and ImageMagick, writes
# demo_picture.{png,gif,jpg}.
# 64 x 48: small enough to see every block and every code, with smooth
# gradients (where JPEG shines and GIF bands) and sharp edges (where
# JPEG rings and PNG's filters show).
import subprocess
from PIL import Image, ImageDraw

W, H = 64, 48
im = Image.new('RGB', (W, H))
# a sky, lighter towards the horizon
im.putdata([(70 + y * 3, 130 + y * 2, 230 - y) for y in range(H) for x in range(W)])
d = ImageDraw.Draw(im)
d.ellipse([44, 5, 57, 18], fill=(255, 220, 60))            # the sun
d.polygon([(0, 34), (18, 24), (36, 33), (52, 26), (63, 31), (63, 47), (0, 47)], fill=(60, 150, 70))  # hills
d.rectangle([12, 28, 27, 41], fill=(235, 230, 215))        # a house
d.polygon([(10, 29), (19.5, 20), (29, 29)], fill=(180, 40, 40))  # its roof
d.rectangle([17, 34, 21, 41], fill=(90, 60, 40))           # its door
d.rectangle([23, 31, 26, 34], fill=(120, 180, 230))        # a window
d.line([(0, 44), (63, 38)], fill=(40, 40, 40), width=1)    # a road
im.save('demo_picture.png')
im.save('demo_picture.jpg', quality=75, subsampling=2)
# the GIF by ImageMagick: PIL's writes LZW codes for 256 colors (a
# minimum code size of 8) even with 16; ImageMagick's start at 5 bits,
# what ImageLzw wants to show
subprocess.run(['convert', 'demo_picture.png', '+dither', '-colors', '16', 'demo_picture.gif'], check=True)
