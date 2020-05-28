#Produces incremental savings results
import logging

import pandas as pd

from cpuc.mylogging import create_logfile
import cpuc.params as params
from cpuc.workbookfunctions import convertwstodf
from cpuc.workbookfunctions import openworkbook
from cpuc.workbookfunctions import getSampleControlFile
from cpuc.createATR import is_number

def createSteps(data = None):

    if data is None:
        df_data = pd.read_csv(params.D0_REPORT_DATA_FILE)
    else:
        df_data = data
    
    xreffile = params.D0_FIELD_CROSS_REF_FILE
    wb = openworkbook(xreffile)
    ws_map = wb['stepeqn']
    #convert to datframe
    df_map = convertwstodf(ws_map, 1)
    fieldcol = 'fieldname'
    eqncol = ['eqn_true', 'eqn_false', 'eqn_condition']
    eqnheaders = [fieldcol]
    eqnheaders.extend(eqncol)

        #some constants for the calculated stuff
    operators = '*/()+-><!==AND&OR|.'
    stringops = 'ANDOR&|'
    dfname = 'df_all_fields'
    df_all_fields = df_data.set_index('ClaimID')
        #pull in conditional
    df_map_eqnfields = df_map[df_map[eqncol].notnull().any(1)]
    df_map_eqnfields = df_map_eqnfields[eqnheaders]
    print(f'all fields column count to start is {len(df_all_fields.columns)}')
    for _, row in df_map_eqnfields.iterrows(): 
        
        parts_true = row[eqncol[0]].split()
        try:
            parts_false = row[eqncol[1]].split()
            parts_cond = row[eqncol[2]].split()
            eqnonly = False
        except:
            eqnonly = True

        nojoin = False
        for p in parts_true:
            if len(parts_true)==1:
                eqn_true = p
                nojoin = True
            elif p not in operators and '==' not in p and '.' not in p and not is_number(p):
                parts_true[parts_true.index(p)] = "{}['{}']".format(dfname,p)         
        if not nojoin:
            eqn_true = ''.join(parts_true)
        nojoin = False
        if not eqnonly:
            for p in parts_false:
                if len(parts_false)==1:
                    eqn_false = p
                    nojoin = True
                elif p not in operators and '==' not in p and not is_number(p):
                    parts_false[parts_false.index(p)] = "{}['{}']".format(dfname,p)         
            if not nojoin:
                eqn_false = ''.join(parts_false)
            nojoin = False
            
            for p in parts_cond:
                if len(parts_cond)==1:
                    eqn_cond = p
                    nojoin = True
                elif p not in operators and 'No' not in p and '==' not in p and '.' not in p and not is_number(p): #maybe change to startswith in operators if can use list
                    parts_cond[parts_cond.index(p)] = "{}['{}']".format(dfname,p) 
                elif p in operators:
                    parts_cond[parts_cond.index(p)] = ' ' + p + ' '
            if not nojoin:
                eqn_cond = ''.join(parts_cond)
    
        #print('eqn true:{}'.format(eqn_true))
        #print('eqn false:{}'.format(eqn_false))
        #print('eqn cond:{}'.format(eqn_cond))

        #df['d'] = df['b'].where(df['b'] < 0, df['c'])
        if eqnonly:
            myargs =  eqn_true
        else:
            myargs =  eqn_true + '.where(' + eqn_cond + ', ' + eqn_false + ')'        
        dftmp = df_all_fields 
        #dftmp.set_index('ClaimID', inplace=True)
        #print(f'all fields colmn count before eval is {len(df_all_fields.columns)}')
        print(f'processing {row[fieldcol]}') #', my args is {myargs}')
        #no idea how the line below is adding col to df_all_fields 
        dftmp[row[fieldcol]] = eval(myargs)
        
        #dftmp = dftmp[row[fieldcol]].reset_index().set_index('ClaimID')
        
        #print(f'all fields colmn count after eval is {len(df_all_fields.columns)}')
        #df_all_fields = df_all_fields.join(dftmp)
        #print(f'all fields colmn count after join is {len(df_all_fields.columns)}')
        #print('ugh')
        #df_all_fields = df_all_fields.merge(dftmp, on='ClaimID')

    print(f'all field shape is {df_all_fields.shape}')

    answerfields = ['ExAnteLifecycleNetkW', 'ExAnteLifecycleNetkWh', 'ExAnteLifecycleNetTherm', 'cdrdatekw', 'cdrdatekwh', 'cdrdatethm', 'cdrntgeligkw', 
        'cdrntgeligkwh', 'cdrntgeligthm', 'cdrulntgeligkw', 'cdrulntgeligkwh', 'cdrulntgeligthm']
    countfields = ['cdrdateineligibleflagkw', 'cdrdateineligibleflagkwh', 'cdrdateineligibleflagthm', 'cdrdatentgineligibleflagkw', 'cdrdatentgineligibleflagkwh', 
        'cdrdatentgineligibleflagthm', 'cdrdatentgulineligibleflagkw', 'cdrdatentgulineligibleflagkwh', 'cdrdatentgulineligibleflagthm']
    answerfieldsPA = answerfields
    answerfieldsPA.append('PA')
    df_all_fieldsShort = df_all_fields[answerfieldsPA]
    summarytable = df_all_fieldsShort.groupby(['PA']).sum()
    csvfile  = params.SAMPLED_SITE_REVIEW_PATH + '\\steps_dbr_SumsbyPA.csv'
    summarytable.to_csv(csvfile)
    
    answerfieldsPA = countfields
    answerfieldsPA.append('PA')
    df_all_fieldsShort = df_all_fields[answerfieldsPA]
    summarytable = df_all_fieldsShort.groupby(['PA']).sum()
    csvfile  = params.SAMPLED_SITE_REVIEW_PATH + '\\steps_dbr_CountsbyPA.csv'
    summarytable.to_csv(csvfile)    
    
    
    '''
    pd.options.display.float_format = '{:20,.0f}'.format
    print(f'step 1 summary {summarytable}')
    df_output = reshapestepssummary(summarytable)
    csvfile  = params.SAMPLED_SITE_REVIEW_PATH + '\\steps_dbr_SumsAll.csv'
    df_output.to_csv(csvfile)
    

    summarytable = df_all_fields[countfields].agg('sum')
    print(f'step 1 count summary {summarytable}')
    df_output = reshapestepssummary(summarytable)
    csvfile  = params.SAMPLED_SITE_REVIEW_PATH + '\\steps_dbr_CountsAll.csv'
    df_output.to_csv(csvfile)
    '''

    #sample only version
    answerfields = ['ExAnteLifecycleNetkW', 'ExAnteLifecycleNetkWh', 'ExAnteLifecycleNetTherm', 'evaleligkw', 'evaleligkwh', 'evaleligthm', 'evalsvgsEligkw', 'evalsvgsEligkwh', 'evalsvgsEligthm', 'evalULsvgeligkw', 
        'evalULsvgeligkwh', 'evalULsvgeligthm', 'evalNTGULsvgeligkw', 'evalNTGULsvgeligkwh', 'evalNTGULsvgeligthm']
    countfields = ['evalineligibleflagkw', 'evalineligibleflagkwh', 'evalineligibleflagthm', 'evalsvgschangeflagkw', 'evalsvgschangeflagkwh', 
        'evalsvgschangeflagthm', 'evalULchangeflagkw', 'evalULchangeflagkwh', 'evalULchangeflagthm', 'evalNTGchangeflagkw',
        'evalNTGchangeflagkwh', 'evalNTGchangeflagthm']
    summarytable = df_all_fields.query('SampledProject == 1')[answerfields].agg('sum')
    #print(f'step 2x summary {summarytable}')
    summarytable = df_all_fields.query('SampledProject == 1')[countfields].agg('sum')
    #print(f'step 2x count summary {summarytable}')

    #ATR version
    answerfields = ['ExAnteLifecycleNetkW', 'ExAnteLifecycleNetkWh', 'ExAnteLifecycleNetTherm', 'atr_eligkw', 'atr_eligkwh', 'atr_eligthm', 'atr_svgsEligkw', 
        'atr_svgsEligkwh', 'atr_svgsEligthm', 'atr_NTGsvgeligkw', 'atr_NTGsvgeligkwh', 'atr_NTGsvgeligthm', 'atr_NTGULsvgeligkw', 'atr_NTGULsvgeligkwh', 'atr_NTGULsvgeligthm']
    countfields = ['atr_ineligibleflagkw', 'atr_ineligibleflagkwh', 'atr_ineligibleflagthm', 'atr_svgschangeflagkw', 'atr_svgschangeflagkwh', 'atr_svgschangeflagthm', 
        'atr_ULchangeflagkw', 'atr_ULchangeflagkwh', 'atr_ULchangeflagthm', 'atr_NTGchangeflagkw', 'atr_NTGchangeflagkwh', 'atr_NTGchangeflagthm']
    
    answerfieldsPA = answerfields
    answerfieldsPA.append('PA')
    df_all_fieldsShort = df_all_fields[answerfieldsPA]
    summarytable = df_all_fieldsShort.groupby(['PA']).sum()
    csvfile  = params.SAMPLED_SITE_REVIEW_PATH + '\\steps_DS_SumsbyPA.csv'
    summarytable.to_csv(csvfile)
    
    answerfieldsPA = countfields
    answerfieldsPA.append('PA')
    df_all_fieldsShort = df_all_fields[answerfieldsPA]
    summarytable = df_all_fieldsShort.groupby(['PA']).sum()
    csvfile  = params.SAMPLED_SITE_REVIEW_PATH + '\\steps_DS_CountsbyPA.csv'
    summarytable.to_csv(csvfile)    

    csvfile = params.D0_REPORT_STEPTABLE_DATA_FILE
    try:
        df_all_fields.to_csv(csvfile, index=True)
    except:
        print ('drat, step file in use. no file created.')

    if data is not None:
        return df_all_fields

def reshapestepssummary(summarytable):
    #reshape summary
    summarytable = pd.DataFrame(summarytable).assign(units = 'kw')
    summarytable['units'] = summarytable['units'].where(~summarytable.index.str.contains('kwh', case=False), 'kwh')
    summarytable['units'] = summarytable['units'].where(~summarytable.index.str.contains('thm', case=False), 'thm')
    summarytable['units'] = summarytable['units'].where(~summarytable.index.str.contains('therm', case=False), 'thm')
    sumtable = summarytable.reset_index()
    sumtable['index'] = sumtable['index'].str.replace('kwh', '', case=False)
    sumtable['index'] = sumtable['index'].str.replace('kw', '', case=False)
    sumtable['index'] = sumtable['index'].str.replace('thm', '', case=False)
    sumtable['index'] = sumtable['index'].str.replace('therm', '', case=False)
    sumunstack = sumtable.set_index(['index', 'units']).unstack('units')
    return sumunstack  

def runCreateSteps():
    
    createSteps()    
    logging.info('done with CreateATR')

if __name__ == '__main__':
    create_logfile('../logs/createATR.log')
    logging.info('Beginning session')    
    createSteps()

