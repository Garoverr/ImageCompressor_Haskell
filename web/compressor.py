#!/usr/bin/env python3

import os
import re
import numpy as np
from PIL import Image
from sys import argv
import subprocess

def parse_pixel_file(file_path):
    """Parse pixel data from the input file."""
    pixels = {}
    width = height = 0
    color = None

    with open(file_path) as f:
        for line in f:
            line = line.strip()

            if '--' in line:
                strColor = f.readline().strip()
                color = list(map(int, re.findall(r'[0-9]+', strColor)))
                f.readline()
                continue

            if color is None:
                raise ValueError('Invalid file format: missing color definition.')

            pos, base_color = line.split(' ', 1)
            x, y = map(int, re.findall(r'[0-9]+', pos))
            pixels[(x, y)] = color

            width = max(width, x)
            height = max(height, y)

    return pixels, width + 1, height + 1

def pixels_to_image(pixels, width, height):
    """Convert pixel data to a PIL image."""
    img_data = np.zeros((height, width, 3), dtype=np.uint8)
    
    for (x, y), color in pixels.items():
        img_data[y, x] = color

    return Image.fromarray(img_data)

def image_to_pixels(image_path):
    """Convert an image to pixel representation."""
    with Image.open(image_path) as img:
        width, height = img.size
        pixels = img.load()

        pixel_data = []
        for y in range(height):
            for x in range(width):
                pixel_data.append(f'({x},{y}) ({pixels[x, y][0]},{pixels[x, y][1]},{pixels[x, y][2]})')

    return '\n'.join(pixel_data)

def run_command(command):
    """Execute a shell command with subprocess."""
    result = subprocess.run(command, shell=True, capture_output=True, text=True)
    if result.returncode != 0:
        raise RuntimeError(f"Command failed: {result.stderr}")
    return result.stdout

def compress_image(image_path, nb_clusters, conv_limit, output_path=None):
    """Main function to compress the image."""
    image_name = os.path.basename(image_path)
    dir_name = os.path.dirname(image_path) or "."

    if output_path is None:
        output_path = os.path.join(dir_name, f'compressed_{image_name}')

    if not os.path.isfile("imageCompressor"):
        raise FileNotFoundError("Executable 'imageCompressor' not found. Please run 'make'.")

    print('Converting image to pixels...')
    pixel_output_path = os.path.join("pixels", f"{image_name}.in")
    pixel_data = image_to_pixels(image_path)

    os.makedirs("pixels", exist_ok=True)
    with open(pixel_output_path, 'w') as pixel_file:
        pixel_file.write(pixel_data)

    print('Compressing pixels...')
    compressed_pixel_output = f"pixels/{image_name}.out"
    run_command(f'./imageCompressor -n {nb_clusters} -l {conv_limit} -f {pixel_output_path} > {compressed_pixel_output}')

    print('Converting compressed pixels back to image...')
    pixels, width, height = parse_pixel_file(compressed_pixel_output)
    compressed_image = pixels_to_image(pixels, width, height)
    compressed_image.save(output_path)

    print(f'Output file: {output_path}')

if __name__ == '__main__':
    
    if "-h" in argv or "--help" in argv:
        print('Usage: ./compress.py image.[jpg/png/bmp] nbClusters convLimit [output_path]')
        exit(0)
    
    if len(argv) not in (4, 5):
        print('Usage: ./compress.py image.[jpg/png/bmp] nbClusters convLimit [output_path]')
        exit(1)

    image_path = argv[1]
    nb_clusters = int(argv[2])
    conv_limit = float(argv[3])
    output_path = argv[4] if len(argv) == 5 else None

    compress_image(image_path, nb_clusters, conv_limit, output_path)
