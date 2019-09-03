# marshal_dico_place.exe

This tool take csv files as input, create place lists from it, and
write them to binary files meant to be used by `gwd` API.

e.g. for a given language `LANG`, provide a `dico_place_LANG.csv` file
containing data as `town,are_code,county,region,country`, and it will
produce these files:

```
dico_place_town_LANG.list
dico_place_area_code_LANG.list
dico_place_county_LANG.list
dico_place_region_LANG.list
dico_place_country_LANG.list
```

Place these files in the `lang` folder used by your geneweb server,
and you are good to go.
