# Script for quickly parsing a (CSV) dataframe through regions constraints and calculating probabilities
import pandas as pd
import importlib
import csv
import argparse

# Main function
def run(data_files, get_probs):

    # Apply region constraints and add as a new column
    data = [pd.read_csv(f, index_col=[0]) for f in data_files]
    try:
        regions = [rc.calculate_regions(*[df.iloc[i] for df in data]) for i in data[0].index]
    except IndexError:
        print("Error: Number of data files must match number of decomposition steps")
        return
    except AttributeError:
        print("Error: Column names in data files must match input variables of decomposed function")
        return
    regions = [tuple(r) for r in regions]

    # Save results as CSV
    with open('regions.csv', 'w') as f:
        f.write(', region')
        for i in range(len(regions)):
            f.write('\n{}, \'{}\''.format(i, "-".join([str(j) for j in regions[i]])))

    # Print and save probabilites if required
    if get_probs:

        # Calculate frequencies and probabilities
        freqs = {r:regions.count(r) for r in regions}
        probs = [['region', 'freq', 'prob']]
        size = len(regions)
        for r in freqs.keys():
            probs.append(["-".join([str(j) for j in r]), freqs[r], freqs[r]/size])

        # Save and print probability results
        with open('probs.csv', 'w') as f:
            for row in probs:
                print("{: <10} {: <10} {: <10}".format(*row))
                f.write('\n\'{}\', {}, {}'.format(*row))

if __name__ == "__main__":

    # Parse arguments and import regions function
    parser = argparse.ArgumentParser()
    parser.add_argument('--data', '--list', nargs='+', required=True, type=str, help="data input files")
    parser.add_argument("--regions", required=True, type=str, help="regions input file")
    parser.add_argument('--probs', dest='probs', action='store_true', default=True, help="report probabilities and frequencies")

    args = parser.parse_args()
    rc = importlib.import_module(args.regions)

    # Run main function
    run(args.data, args.probs)