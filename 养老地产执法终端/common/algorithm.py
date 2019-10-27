import configparser
import os


def get_config(key):
    config = configparser.ConfigParser()
    config_ini = os.path.join(os.getcwd(), 'config.ini')
    if not os.path.exists(config_ini):
        config_ini = os.path.abspath(os.path.join(os.getcwd(), '../../../common/config.ini'))
    config.read(config_ini)
    return config[config.default_section][key]


if __name__ == '__main__':
    print(get_config('ip'))
