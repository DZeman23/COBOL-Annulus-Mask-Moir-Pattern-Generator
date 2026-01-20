import numpy as np
from PIL import Image
import os

def convert_dat_to_image_filtered(input_filename, output_filename):
    print(f"Processing {input_filename}...")
    
    parsed_data = []
    max_x = 0
    max_y = 0
    
    try:
        with open(input_filename, 'rb') as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                
                # Split by null byte
                parts = line.split(b'\x00')
                
                if len(parts) >= 3:
                    try:
                        x = int(parts[0])
                        y = int(parts[1])
                        
                        # Handle brightness value
                        # Decode bytes to string to check for '#'
                        b_str = parts[2].decode('utf-8', errors='ignore').strip()
                        
                        # Check if the value is marked with '#' or '####'
                        if '#' in b_str:
                            brightness = 0  # Black out the pixel
                        else:
                            brightness = int(b_str)
                        
                        # Store data
                        parsed_data.append((x, y, brightness))
                        
                        # Update dimensions
                        if x > max_x: max_x = x
                        if y > max_y: max_y = y
                        
                    except ValueError:
                        continue
        
        if max_x == 0 or max_y == 0:
            print("Error: No valid data found.")
            return

        print(f"Detected dimensions: {max_x} x {max_y}")

        # Initialize image array (height, width)
        image_data = np.zeros((max_y, max_x), dtype=np.uint8)
        
        # Populate the array
        for x, y, brightness in parsed_data:
            # Adjust 1-based coordinates to 0-based
            if 1 <= x <= max_x and 1 <= y <= max_y:
                image_data[y-1, x-1] = brightness
        
        # Save image
        img = Image.fromarray(image_data, mode='L')
        img.save(output_filename)
        print(f"Successfully saved image to {output_filename}")
        
    except FileNotFoundError:
        print(f"Error: The file {input_filename} was not found.")
    except Exception as e:
        print(f"An error occurred: {e}")
if __name__ == "__main__":
    # This script generates a grayscale image (Mode 'L')
    # 0 will naturally be black, and 255 will be white.
    convert_dat_to_image_filtered('new_coords.dat', 'reconstructed_image.png')

