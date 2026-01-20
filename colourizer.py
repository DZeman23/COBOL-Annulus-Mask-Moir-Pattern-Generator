import numpy as np
import matplotlib.pyplot as plt
from PIL import Image
import os

def convert_dat_to_color_image(input_filename, output_filename, colormap_name='turbo'):
    print(f"Processing {input_filename}...")
    
    parsed_data = []
    max_x = 0
    max_y = 0
    valid_values = [] # Store valid brightness values to determine range
    
    try:
        with open(input_filename, 'rb') as f:
            for line in f:
                line = line.strip()
                if not line:
                    continue
                
                parts = line.split(b'\x00')
                
                if len(parts) >= 3:
                    try:
                        x = int(parts[0])
                        y = int(parts[1])
                        
                        # Handle brightness check
                        b_str = parts[2].decode('utf-8', errors='ignore').strip()
                        
                        is_blackout = False
                        if '#' in b_str:
                            brightness = 0
                            is_blackout = True
                        else:
                            brightness = int(b_str)
                            valid_values.append(brightness)
                        
                        parsed_data.append((x, y, brightness, is_blackout))
                        
                        if x > max_x: max_x = x
                        if y > max_y: max_y = y
                        
                    except ValueError:
                        continue
        
        if max_x == 0 or max_y == 0:
            print("Error: No valid data found.")
            return

        print(f"Dimensions: {max_x} x {max_y}")
        
        # Determine Value Range for Normalization
        if valid_values:
            v_min = min(valid_values)
            v_max = max(valid_values)
        else:
            v_min, v_max = 0, 255
            print(f"Mapping brightness range {v_min}-{v_max} to colors.")

        # --- MODIFIED SECTION START ---
        # Get the original colormap
        original_cmap = plt.get_cmap(colormap_name)
        
        # Create a custom colormap that starts with Black
        from matplotlib.colors import ListedColormap
        
        # Sample 256 colors from the original map
        new_colors = original_cmap(np.linspace(0, 1, 256))
        
        # Force the lowest value (index 0) to Black (R=0, G=0, B=0, Alpha=1)
        new_colors[0] = np.array([0, 0, 0, 1]) 
        
        # Create the new custom colormap
        cmap = ListedColormap(new_colors)
        # --- MODIFIED SECTION END ---

        # Initialize RGB image array (Height, Width, 3 channels)




        # Initialize RGB image array (Height, Width, 3 channels)
        image_data = np.zeros((max_y, max_x, 3), dtype=np.uint8)
        
        for x, y, val, is_blackout in parsed_data:
            if 1 <= x <= max_x and 1 <= y <= max_y:
                if is_blackout:
                    # Force Black for blackout pixels
                    image_data[y-1, x-1] = [0, 0, 0]
                else:
                    # Normalize value between 0.0 and 1.0
                    if v_max > v_min:
                        norm = (val - v_min) / (v_max - v_min)
                    else:
                        norm = 0.5 # Default if all values are identical
                    
                    # Get RGBA color from map
                    rgba = cmap(norm)
                    
                    # Convert 0-1 float to 0-255 int
                    r = int(rgba[0] * 255)
                    g = int(rgba[1] * 255)
                    b = int(rgba[2] * 255)
                    
                    image_data[y-1, x-1] = [r, g, b]
        
        # Save image
        img = Image.fromarray(image_data, mode='RGB')
        img.save(output_filename)
        print(f"Successfully saved image to {output_filename}")
        
    except FileNotFoundError:
        print(f"Error: The file {input_filename} was not found.")
    except Exception as e:
        print(f"An error occurred: {e}")

if __name__ == "__main__":
    # You can change 'turbo' to 'viridis', 'plasma', 'jet', etc.
    convert_dat_to_color_image('new_coords.dat', 'reconstructed_color.png', colormap_name='turbo')