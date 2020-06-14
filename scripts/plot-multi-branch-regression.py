import click
import subprocess
import os
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

@click.command()
@click.option("--files")
def run_program(files):
    for f in os.listdir(files):
        if f.endswith(".csv"):
            full_file = files + "/" + f
            csv = pd.read_csv(full_file, names=['test_case','branch','mean','std'])
            print(csv)
            branches = csv['branch'].unique()
            bar_width = 1 / (len(branches)+1)
            num_test_cases = len(csv[csv['branch'] == branches[0]])
            uniq_test_cases = csv['test_case'].unique()
            x_vals = np.arange(num_test_cases)
            
            for branch in branches:
                mean_time = csv[csv['branch'] == branch]['mean']
                plt.bar(x_vals, mean_time, width=bar_width, label=branch)
                x_vals = [x + bar_width for x in x_vals]

            plt.xticks([r for r in range(len(uniq_test_cases))], uniq_test_cases)
            plt.title(f)
            plt.xlabel("test case")
            plt.ylabel("time")
            plt.legend()
            plt.savefig(full_file + ".pdf")
            plt.show()

if __name__ == "__main__":
    run_program()
