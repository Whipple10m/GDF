# GDF, the Granite Data Format

`GDF` is the data format used by the Whipple 10m during the period from 1994 to the end of its life in 2011. `GDF` was written by Joachim Rose from the University of Leeds.

`GDF` is based on the `ZEBRA` file and memory management system developed by CERN. It provides as a layer that insulates the user from having to interact with `ZEBRA` directly, translating information between the FORTARN structures used by the Whipple DAQ system (and analysis code) and *data banks* stored in the `ZEBRA/FZ` files. `GDF` supports six data structures used to store the Whipple 10m data:

- `gdf_run_t`: Run header providing high-level information on the run configuration, such as the start and end times of the run.
- `gdf_fr10_t`: 10m frames providing on-the-fly calibration measurements used until mid-1997.
- `gdf_ev10_t`: 10m events providing measurements of extensive air-showers, and flat-fielding and noise calibration data.
- `gdf_track_t`: Information provided by the telescope control (tracking) system giving the position of the telescope and target in the sky.
- `gdf_hv_t`: Status, voltage settings, and current and voltage measurements made by the high-voltage system.
- `gdf_ccd_t`: Measurments of the stars in the field of view of an optical CCD monitoring the pointing of the telescope.

Data is collected by the Whipple data acquisition system, `Granite`, assembled into the relevant FORTAN data structure and passed to the `GDF` library, which copies the data into the `ZEBRA` *data banks*, which are named `'RUUR'`, `'FTTF'`, `'ETTE'`, `'TRRT'`, `'HVVH'`, and `'CCCC'` (the palendromic names making it easier to recognise the bank names on little-endian or big-endian systems). `GDF` then calls `ZEBRA`, which marshalls the data into logical and physical records, proving redundant header information that can be used to resychnoise the datastream if necessary.

In reading the operations are reversed: `ZEBRA` reads and unpacks the physical and logical records, `GDF` recognises and copies the information from the *data banks* into the FORTRAN structures, which can then be used by the data analyis software.

The [GDF manual](https://github.com/Whipple10m/GDF/blob/main/gdf.pdf), included here, provides more information.

This is version 0.83 of GDF.
