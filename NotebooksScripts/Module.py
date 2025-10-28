#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Copyright 2024 University of Oxford. All Rights Reserved.
The authors, being Dr Scot Wheeler, have asserted their moral rights.

This is the module docstring. It can be used to introduce the purpose and
key functionality of the module. It will appear in documentation/help.
As an example, in the spyder help window (top right subwindow be default), 
type scipy.optimize to see an example of the docstring for the 
optimize module of the scipy library.

The very first line of the script is an optional shebang line. It tells the 
operating system which interpreter to use to execute the code if the script 
was treated as a stadalone executable.
"""

# Complete the script by filling in the missing code sections marked with <---.

# import required libraries
import math
import numpy as np
import pandas as pd

# =============================================================================
# Calculate PV array size function
# =============================================================================

def calc_pv_array_size(building_width, building_length,
                       roof_angle, pv_width, pv_height,
                       pv_power):
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


    return total_capacity_kw, max_panels # <--- return the total PV capacity in kW and number of panels

# =============================================================================
# Example PV class
# =============================================================================
class PV():
    """
    PV object

    Parameters
    ----------
    pv_id : str
        Unique identifier for the PV asset.
    capacity_factor : list or numpy array
        Capacity factor (kW/kWp) profile for the PV asset.
    dt : float
        Time step in hours.
    peak_power_kw : float, optional
        Peak power of the PV asset in kW. The default is 4.0 kW.
    """

    def __init__(self, pv_id, capacity_factor, dt, peak_power_kw=4.0,):
        self.id = pv_id
        self.capacity_factor = capacity_factor
        self.peak_power_kw = peak_power_kw
        self.T = len(capacity_factor)
        self.dt = dt

        # we can call the method to calculate the output profile on initialisation
        self.pv_output = self.pv_power_output()

    def pv_power_output(self):
        """
        Calculate the PV power output profile.

        Returns
        -------
        pv_output : list
            Power output profile of the PV asset in kW.
        """
        self.pv_output_p = self.capacity_factor * self.peak_power_kw
        return self.pv_output_p

# =============================================================================
# Basic conditional battery class
# =============================================================================
class Storage():
    """
    Storage object

    Parameters
    ----------
    id : str
        Unique identifier for the battery asset.
    T : int
        Total number of time periods.
    max_soc : float, optional
        Maximum state of charge (kWh). The default is 10.0 kWh.
    min_soc : float, optional
        Minimum state of charge (kWh). The default is 1.0 kWh.
    max_power : float, optional
        Maximum power the battery can handle (kW). The default is 15.0 kW.
    efficiency : float, optional
        Base battery efficiency. The default is 0.98.
    self_discharge : float, optional
        Battery degradation factor. The default is 0.01.
    soc_0 : float, optional
        Initial state of charge (kWh). The default is 5.0 kWh.
    model : str, optional
        Battery model name. The default is "Tesla Powerwall".
    """

    def __init__(self, id, T, dt, max_soc=14.0, min_soc=0.5, max_power=11.0,
                  efficiency=0.99, self_discharge=0.01, soc_0=5.0,
                    model="Tesla Powerwall"):
        self.id = id
        self.T = T # Total number of time periods
        self.dt = dt # Time step in hours
        self.max_soc = max_soc  # Maximum state of charge (kWh)
        self.min_soc = min_soc   # Minimum state of charge (kWh)
        self.max_power = max_power # Maximum power the battery can handle (kW)
        self.efficiency = efficiency    # Base battery efficiency
        self.self_discharge = self_discharge    # Battery degradation factor
        self.soc_0 = soc_0  # Initial state of charge (kWh)
        self.model = model  # Battery model name

        self.storage_power = [0] * self.T  # Initialize storage power list
        self.soc_E = [self.soc_0] * self.T  # Initialize state of charge list
        

    def battery_charge_action(self, demand_P, t):
        """
        This function decides if to charge or discharge a battery based on a 
        simple conditional battery model, within a single time period t of length dt.
        It returns the net demand after the storage action.
        
        Parameters
        ----------
        demand_P : float
            The power demand (units of power e.g. kW)
        t : int
            The current time period index.
            
        Returns
        -------
        net_demand : float
            The net demand post battery operation.
        """
        # add you conditional battery model from above, adjusting to include
        # the self keyword.
        if demand_P > 0: # positive demand -> discharge storage
            # internal storage power is negative when discharging
            if t == 0:
                bat_P_ideal_t = -1 * min(demand_P, self.max_power, (self.soc_0-self.min_soc) / self.dt)
            else:
                bat_P_ideal_t = -1 * min(demand_P, self.max_power, (self.soc_E[t-1]-self.min_soc) / self.dt)

            # actually requires more internal power due to efficiency losses
            bat_P_real_t = bat_P_ideal_t - math.fabs(bat_P_ideal_t) * (1-self.efficiency)
        elif demand_P < 0:  # negative demand -> charge storage
            # internal storage power is positive when charging
            if t == 0:
                bat_P_ideal_t = min(-1*demand_P, self.max_power, (self.max_soc - self.soc_0) / self.dt)
            else:
                bat_P_ideal_t = min(-1*demand_P, self.max_power, (self.max_soc - self.soc_E[t-1]) / self.dt)

            # less internal charging power due to efficiency losses
            bat_P_real_t = bat_P_ideal_t - math.fabs(bat_P_ideal_t) * (1-self.efficiency)
        else: # zero demand -> no storage operation
            bat_P_ideal_t = 0
            bat_P_real_t = 0
            
        # update the state of charge
        if t == 0:
            self.soc_E[t] = self.soc_0 + bat_P_real_t * self.dt
        else:
            self.soc_E[t] = self.soc_E[t-1] + bat_P_real_t * self.dt

        # calcualate net demand
        net_demand_P = demand_P + bat_P_ideal_t

        # update storage power
        self.storage_power[t] = bat_P_real_t

        # apply soc degradation
        if t==0:
            self.soc_E[0] = self.soc_E[0] - ((self.soc_0 + self.soc_E[t])/2) * self.self_discharge
        else:
            self.soc_E[t] = self.soc_E[t] - ((self.soc_E[t-1] + self.soc_E[t])/2) * self.self_discharge
        
        return net_demand_P


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
    
    
    
    pass