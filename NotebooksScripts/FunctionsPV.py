"""
Functions, Classes and Modules Tutorial
This file demonstrates Python functions.

Learning objectives:
- Defining and using functions

Complete the script by filling in the missing code sections marked with <---.

@author: PLACE YOUR NAME HERE
"""

# Import any necessary libraries
import math
import pandas as pd
import numpy as np
import os

# <--- Define a function to size a PV system based on building dimensions and panel specifications
def calculate_pv_size(building_width, building_length, roof_angle, pv_width, pv_height, pv_power):
    """
    Calculate the total PV capacity that can be installed on a building's roof (with no overhang).

    Parameters
    ----------
    building_width : float
        The width of the building in metres.
    building_length : float
        The length of the building in metres.
    roof_angle : float
        The angle of the roof in degrees.
    pv_width : float
        The width of a single PV panel in mm.
    pv_height : float
        The height of a single PV panel in mm.
    pv_power : float
        The power rating of a single PV panel in Watts.

    Returns
    -------
    total_capacity_kw : float
        The total PV capacity that can be installed on the roof in kW.
    num_panels : int
        The number of PV panels that can fit on the roof.
    """
    
    # orientation 1: panel width along building length

    n_panels_length1 = building_length // (pv_width / 1000)
    n_panels_height1 = ((building_width/2) / math.cos(math.radians(roof_angle))) // (pv_height / 1000)

    num_panels1 = n_panels_length1 * n_panels_height1

    # orientation 2: panel height along building length

    n_panels_length2 = building_length // (pv_height / 1000)
    n_panels_height2 = ((building_width/2) / math.cos(math.radians(roof_angle))) // (pv_width / 1000)

    num_panels2 = n_panels_length2 * n_panels_height2

    # determine the maximum number of panels from both orientations
    max_panels = max(num_panels1, num_panels2)

    # calculate the total PV capacity in kW
    total_capacity_kw = max_panels * pv_power / 1000

    return total_capacity_kw, max_panels

if __name__ == "__main__":
    # =============================================================================
    # This section is a common way to incorporate code that you want to run if the 
    # script is executed directly, but will be ignored if the script is 
    # imported as a module into another. 
    # 
    # It separates the executable part of the script from the part that defines
    # reusable components e.g. functions.
    # 
    # This is useful way of testing the code or providing examples of how to 
    # use the code.
    # =============================================================================
    holywell_house_dimensions = {
        'width_m': 16.0,
        'length_m': 30.0,
        'roof_angle_deg': 22.0
    }
    panel_specs = {
        'width_mm': 1690,
        'height_mm': 1046,
        'power_watts': 400
    }
    # <--- call the calculate_pv_size function with appropriate arguments
    pv_capacity_kw, num_panels = calculate_pv_size( 
        holywell_house_dimensions['width_m'],
        holywell_house_dimensions['length_m'],
        holywell_house_dimensions['roof_angle_deg'],
        panel_specs['width_mm'],
        panel_specs['height_mm'],
        panel_specs['power_watts']
    )

    print(f'Number of PV panels: {num_panels}')
    print(f'Total PV capacity: {pv_capacity_kw} kW')
