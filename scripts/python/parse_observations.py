import sys
import csv


def inside_bounds(lon, lat):
    min_lat, max_lat = (39.86966235384007, 45.79239431452086)
    min_lon, max_lon = (-75.49804687250003, 73.80615234125003)
    return (min_lon <= lon <= max_lon) and (min_lat <= lat <= max_lat)


def parse_old(lines):
    coord_index = 75
    obs_index = 155
    inside_bounds(2,2)
    results = []


    for line in lines[1:]:
        if line[coord_index] == "":
            continue

        lat, lon = map(float, line[coord_index].split(","))
        if not inside_bounds(lon, lat):
            continue

        observed = line[obs_index] == "Positive"
        results.append([lat, lon, observed])

    return results

def parse_new(lines):
    lon_index = 0
    lat_index = 1
    obs_index = 10
    results = []
    for line in lines[1:]:
        lon, lat = map(float, [line[lon_index], line[lat_index]])
        observed = line[obs_index] == "present"

        if not inside_bounds(lon, lat):
            continue

        results.append([lon, lat, observed])
    return results


def parse_csv(file):
    csvreader = csv.reader(file, delimiter=",", quotechar='"')
    lines = [line for line in csvreader]
    if lines[0][0] == "objectid":
        return parse_old(lines)

    else:
        return parse_new(lines)



def write_results(results):
    with open("glossy_observations.csv", "w") as f:
        f.write("lon,lat,present\n")
        joined = "\n".join(map(lambda x: ",".join(map(str, x)), results))
        f.write(joined + "\n")


if __name__ == '__main__':
    final_results = []
    for file in sys.argv[1:]:
        with open(file, encoding="latin-1") as f:
            final_results.extend(parse_csv(f))

    write_results(final_results)



