from PIL import Image
import numpy as np
import pandas as pd
import os

path = 'image.png'  # Make sure this matches your file name
output_filename = 'image_coordinates.dat'

if not os.path.exists(path):
    print(f"Error: '{path}' not found.")
else:
    # 1. Open image and convert to grayscale
    img_obj = Image.open(path).convert('L')
    
    # 2. Convert to NumPy array
    im_array = np.array(img_obj)

    # --- LOGIC: HANDLE DUPLICATE MAX VALUES ---
    max_val = np.max(im_array)
    # Find all coordinates where the pixel equals the max value
    max_y_locs, max_x_locs = np.where(im_array == max_val)
    
    count = len(max_y_locs)

    if count > 1:
        if count == 2:
            # If exactly two duplicates, randomly pick one and add 1
            idx = np.random.randint(0, count)
            im_array[max_y_locs[idx], max_x_locs[idx]] += 1
        else:
            # If more than two, calculate average position
            avg_y = np.mean(max_y_locs)
            avg_x = np.mean(max_x_locs)
            
            # Round down (floor) to get integer indices
            target_y = int(np.floor(avg_y))
            target_x = int(np.floor(avg_x))
            
            # Set the average position pixel to be the new unique max
            # (max_val + 1) ensures it is strictly the highest
            im_array[target_y, target_x] = max_val + 1
            
    # --- END LOGIC ---

    height, width = im_array.shape

    # 3. Create Coordinate Grids
    y_grid, x_grid = np.indices((height, width))

    # --- ADJUSTMENT: 1-BASED INDEXING ---
    # Add 1 to both grids so coordinates start at 1 instead of 0
    y_grid += 1
    x_grid += 1

    # 4. Flatten the arrays and create a DataFrame
    df = pd.DataFrame({
        'x_coordinate': x_grid.ravel(),
        'y_coordinate': y_grid.ravel(),
        'pixel_value': im_array.ravel()
    })

    # 5. Write to .dat file
    # header=False removes the top row labels
    df.to_csv(output_filename, sep=',', index=False, header=False)

    print(f"Success! Saved {len(df)} pixels to {output_filename}")
    print("Note: Coordinates are 1-based and headers have been removed.")