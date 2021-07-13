import argparse
import shutil
import os

my_parser = argparse.ArgumentParser(description='Do my homework in Distributed Functional Programming (046273)')
my_parser.add_argument('--hw', type=int, help='the number of the hw', required=True, choices=range(1, 4))
my_parser.add_argument('--id1', type=int, help='the id of the first student', required=True)
my_parser.add_argument('--id2', type=int, help='the id of the second student', required=False)
my_parser.add_argument('--authors', type=str, help="the name(s) of the student(s)", required=False, nargs='+')


def clone_hw(args):
    file_names = [
        ['game.erl', 'shapes.erl'],
        ['matrix_server.erl', 'matrix_utils.erl', 'server_supervisor.erl'],
        ['loadBalance.erl', 'server_instance.erl', 'supervisor_instance.erl']]
    new_files = {}
    for file_name in file_names[args.hw-1]:
        with open(f'./HW_{args.hw}/{file_name}') as file:
            new_files[file_name] = file.readlines()
    return new_files    

def change_author(new_files, authors = None):
    for file_name in new_files.keys():
        for i in range(len(new_files[file_name])):
            if new_files[file_name][i].startswith('-author('):
                new_files[file_name][i] = f'-author("{" & ".join(authors)}").\n' if authors != None else ''
                break
    return new_files

def readme(args: argparse.Namespace):
    return [f'{args.id1}\n', f'{args.id2}' if args.id2 != None else '']

def save_files(new_files, dir_name):
    os.makedirs(os.path.dirname(dir_name), exist_ok=True)

    for file_name in new_files.keys():
        with open(dir_name + file_name, 'w') as f:
            f.writelines(new_files[file_name])

def zip_name(args: argparse.Namespace):
    return f'ERLANG_HW{args.hw}_{args.id1}_{args.id2}' if args.id2 != None else f'ERLANG_HW{args.hw}_{args.id1}'


def main(args):
    new_files = clone_hw(args)
    new_files = change_author(new_files, args.authors)
    new_files['readme.txt'] = readme(args)

    dir_name = './temp/'
    save_files(new_files, dir_name)
    shutil.make_archive(zip_name(args), 'zip', dir_name)
    shutil.rmtree(dir_name)
    pass

if __name__ == '__main__':
    args = my_parser.parse_args()
    main(args)
