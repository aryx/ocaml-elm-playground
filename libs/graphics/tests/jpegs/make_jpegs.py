# claude: how Unit_jpeg's fixtures were made (python3 make_jpegs.py, with
# PIL over libjpeg-turbo 2.1.5): the JPEGs, and the pixels libjpeg
# decodes them to, as PNGs (read by our own Png)

from PIL import Image, ImageDraw

# 45 x 29: blocks and MCUs cut at the right and bottom edges; gradients,
# and sharp color edges where subsampling shows
im = Image.new('RGB', (45, 29))
im.putdata([(x * 255 // 44, y * 255 // 28, 128) for y in range(29) for x in range(45)])
d = ImageDraw.Draw(im)
d.rectangle([5, 4, 18, 14], fill=(220, 30, 30))
d.ellipse([22, 8, 40, 26], fill=(20, 200, 60))
d.line([0, 28, 44, 0], fill=(255, 255, 255), width=2)
im.save('source.png')

im.save('q75_444.jpg', quality=75, subsampling=0)
im.save('q75_422.jpg', quality=75, subsampling=1)
im.save('q75_420.jpg', quality=75, subsampling=2)
im.convert('L').save('gray.jpg', quality=75)
im.save('restart.jpg', quality=90, subsampling=2, optimize=True, restart_marker_blocks=3)
im.save('progressive.jpg', quality=75, progressive=True)
im.convert('CMYK').save('cmyk.jpg', quality=75)
for f in ['q75_444', 'q75_422', 'q75_420', 'gray', 'restart']:
    Image.open(f + '.jpg').convert('RGBA').save(f + '.expected.png')
