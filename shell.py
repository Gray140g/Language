#import os
import basic

#os.system('CLS')

fn = "example.basic"
start_text = f"Run(\"{fn}\")"

basic.run(fn, start_text)

#while True:
    #text = input('basic > ')
    #if text.strip() == "": continue
    #result, error = basic.run(fn, text)

    #if error:
        #print(error.as_string())
    #elif result:
        #if len(result.elements) == 1:
            #print(repr(result.elements[0]))
        #else:
            #print(repr(result))