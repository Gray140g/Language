#import os
import basic

#os.system('CLS')

while True:
    text = input('basic > ')
    result, error = basic.run('<nofile>', text)

    if error:
        print(error.as_string())
    elif result:
        print(result)