# -*- coding: utf-8 -*-
"""
Created on Fri Jun  3 14:57:13 2016

@author: xiaoydai
"""


################################ extracting Rules ##################################

"""
need to complete:
    automatically find the POJO file name, convert it to txt.file then read it in.
"""


def pojo_extractor(path):
    file = open(path)
    fileByLine = file.readlines()
    fileByLetter = ''.join(fileByLine)
    raw_Rules = getRules(fileByLetter)
    [feature, reference] = getReference(fileByLine)

# write and read Rules 
# each rule may store the references for the same condition, which causes problem with cleanRules.
# when update one condition, we may change other condition in the rule, as they have the same reference.
# So we using the folowing tricks to solve this, by writing and then reading the raw_Rules.
    import json
    with open("/tmp/hehe.json","w") as f:
        json.dump(raw_Rules, f)     
    with open("/tmp/hehe.json") as f:
        Rules = json.load(f)

# clean rules, i.e. merging identical rules and rules split on the same continous feature
    Rules = cleanRules(Rules, feature, reference)

    return [feature, reference, Rules]

######################### getRules ###################################

def getRules(textWhole):

    Rules = []    
    readIndex = 0
    textSetTable = []
    textModel = []   
    [startModel, endModel, startSetTable, endSetTable] = [0, 0, 0, 0]
    nTextTree = 0 #number of 'Tree' in the text as each treeModel has two 'Tree' in front    
    
    while(readIndex < len(textWhole)-4):       
        if(textWhole[readIndex:(readIndex+4)] == 'Tree'):
            nTextTree = nTextTree + 1       
        if((textWhole[readIndex:(readIndex+4)] == 'Tree' and nTextTree%2 == 0 ) or readIndex == len(textWhole)-5):
            endSetTable = readIndex                      
            textSetTable = textWhole[startSetTable:endSetTable]            
            textModel = textWhole[startModel:endModel]                      
            setTable = getSetTable(textSetTable)
            newRules = getRulesInTree(textModel, [], setTable)
            Rules.extend(newRules)           
        if(textWhole[readIndex:(readIndex+6)] == 'pred ='):
            add = 1
            while(textWhole[readIndex+add] != '(' and readIndex+add < len(textWhole)-1):
                add = add+1
            startModel = readIndex + add
        if(textWhole[readIndex:(readIndex+12)] == 'return pred;'):
            endModel = readIndex - 6
            startSetTable = readIndex        
        readIndex = readIndex + 1
    return Rules

  
def getRulesInTree(text, previousRule, setTable):
    """
    text: a list of words
    Rules: a list of rule's
    previousRules: a rule corresponding to the root node
    rule: a list of conditions
    condition: a list of [0 or 1, int, realNum or set, 0 or 1]
        1st entry:
            0: continuous
            1: categorical
        2nd entry:
            the ith feature
        3rd entry:
            realNum: cutoff for continuous feature
            set: containingSet for categorical feature
        4th entry:
            0: smaller or notIn, i.e. go left
            1: larger or in, i.e. go right
    setTable: 
    """          
    if len(text)<1:
        return []       
    [condition1, condition2, text1, text2] = getCondition(text, setTable)
    rule1 = previousRule[:]
    rule1.append(condition1)
    rule2 = previousRule[:]
    rule2.append(condition2)    
    
    if '?' not in text1:
        if '?' not in text2:
            return [rule1, rule2] #in this case, condition1 and condition2 are basically equivalent    
        else:
            return getRulesInTree(text2, rule2, setTable) + [rule1]
    else:
        if '?' not in text2:
            return getRulesInTree(text1, rule1, setTable) + [rule2]
        else:
            return getRulesInTree(text1, rule1, setTable) + getRulesInTree(text2, rule2, setTable) + [rule1, rule2]
    
    
def decimalToBinary(textNum):
    """
    Convert a decimal to a reverse eight-digit binary    
    """  
    binary = bin(int(textNum)%256)[2:] #after bin, there will be '0b' in front 
    binary = '0' * (8-len(binary)) + binary      
    return binary[::-1]
             

def getSetTable(textSetTable):
    """
    table: a list of setRef
    setRef: a list of int representing spliting subset of a condition for categorical feature
    """    
    table = []
    newByte = textSetTable.find('new byte')
    while(newByte != -1):
        start = textSetTable[newByte:].find('{') + newByte
        end = textSetTable[newByte:].find('}') + newByte
        textNum = textSetTable[start+1:end].split() #still has ',', e.g. '8,'
        textNum[-1] += ','
        textBinary = [decimalToBinary(i[:-1]) for i in textNum]
        textSet = ''.join(textBinary)
        setRef = [i for i in range(len(textSet)) if textSet.startswith('1', i)]        
        table.append(setRef)
        
        if(textSetTable[start + 3:].find('new byte') == -1):
            break        
        newByte = textSetTable[start + 3:].find('new byte') + start + 3
    return table  
        
        
              
def getCondition(text, setTable):
    """        
    split a text according to ()?():() syntax
    text: must be in the format '(...?...:...)'
    """  
    readIndex = 1
    [startCondition, endCondition] = [1, 1]
    [startText1, endText1, startText2, endText2] = [0, 0, 0, len(text)-1]
    # first split into three parts
    shiftA = 0 #to moniter '(' and ')' for condition, i.e. get '?'
    shiftB = 0 #to moniter text1 and text2 after '?'
    shiftC = 0 #to moniter '(' and ')' for text1 and text2, i.e. get ':'
    while(readIndex < len(text)):
        if(text[readIndex] == '(' and shiftB == 0):
            shiftA = shiftA + 1
        if(text[readIndex] == ')' and shiftB == 0):
            shiftA = shiftA - 1
        if(text[readIndex] == '?' and shiftB == 0 and shiftA == 0):
            endCondition = readIndex - 1
            shiftB = 1
            k = 1 #to determine the next non-null entry
            while(text[readIndex+k] == '\n' or text[readIndex+k] == ' '):            
                k = k + 1
            startText1 = readIndex + k          
        if(text[readIndex] == '(' and shiftB == 1):
            shiftC = shiftC + 1
        if(text[readIndex] == ')' and shiftB == 1):
            shiftC = shiftC - 1
        if(text[readIndex] == ':' and shiftB == 1 and shiftC == 0):
            endText1 = readIndex - 1    
            k = 1 #to determine the next non-null entry
            while(text[readIndex+k] == '\n' or text[readIndex+k] == ' '):            
                k = k + 1
            startText2 = readIndex + k 
        # print([shiftA,shiftB,shiftC]) #for debugging
        readIndex = readIndex + 1   
        
    textCondition = text[startCondition:(endCondition)]
    text1 = text[startText1:(endText1)]
    text2 = text[startText2:(endText2)]    
    [condition1, condition2] = interpretCondition(textCondition, setTable)    
    return [condition1, condition2, text1, text2]
    
    
def interpretCondition(textCondition, setTable):
    """
    generate a condition for a feature, represented by [type, indexOfFeature, reference num or set, go left or right]
    
    featureType:
    0:  x_i < c
    1:  x_i in {c_1, ..., c_k}
    2:  x_i != c
    -1: x_i is NA or x_i < c
    3:  x_i is NA
    
    """
    featureType = 0
    featureIndex = 0
    featureRef = 0  
    if '<' in textCondition:
        featureType = 0
        idx1 = textCondition.find('data')
        idx2 = min(textCondition[idx1:].find(' '), textCondition[idx1:].find(']'))        
        featureIndex = int(textCondition[(idx1+5):(idx2+idx1)])            
        idx = textCondition.find('<') #to make sure 'f' we found is behind of '<'
        cutoff = textCondition[(idx+1):(textCondition[idx:].find('f')+idx)]         
        featureRef = float(cutoff) #a float 
        if 'isNaN' in textCondition:
            featureType = -1
    else:
        if '!=' in textCondition and 'GRPSPLIT' not in textCondition:
            featureType = 2
            idx1 = textCondition.find('data')        
            featureIndex = int(textCondition[(idx1+5):(textCondition[idx1:].find(']')+idx1)])   
            idx2 = textCondition.find('=') #to make sure 'f' we found is behind of '='
            point = textCondition[(idx2+2):(textCondition[idx2:].find('f')+idx2)]
            featureRef = float(point) #a float
        elif 'GRPSPLIT' in textCondition:
            featureType = 1
            idx1 = textCondition.find('data')
            find1 = textCondition[idx1:].find(' ')            
            find2 = textCondition[idx1:].find(']')
            if find1 < 0:
                find1 = 999999
            if find2 < 0:
                find2 = 999999
            idx2 = min(find1, find2)
            featureIndex = int(textCondition[(idx1+5):(idx1+idx2)])   
            idx3 = textCondition.find('GRPSPLIT')
            setIndex = int(textCondition[(idx3+8):(textCondition[idx3:].find(',')+idx3)])          
            idx4 = textCondition[idx3:].find(',')+idx3+1
            numZeros = int(textCondition[(idx4+1):(textCondition[idx4:].find(',')+idx4)]) #number of zeros in front, i.e. bitoff        
            featureRef = [x + numZeros for x in setTable[setIndex]] #a set 
        else: # discard unusual splits
            featureType = 3
            idx1 = textCondition.find('data')
            idx2 = textCondition[idx1:].find(']')
            featureIndex = int(textCondition[(idx1+5):(idx2+idx1)])
            featureRef = 0
        
    condition1 = [featureType, featureIndex, featureRef, 0]
    condition2 = [featureType, featureIndex, featureRef, 1]
    return [condition1, condition2]


######################### getReference ###################################

def refine(textWhole):
    start = textWhole.index('// The class representing training column names\n')
    end = textWhole.index('  static final double score0(double[] data) {\n')
    return textWhole[start:(end-7)]


def getReference(textWhole):
    """
    featureNames: [nameOfFeature1, nameOfFeature2, ... , nameOfFeatureP]

    referenceNames: [
                        ["", "", ... , ""], #referecneNames for feature1
                            .
                            .
                            .
                        ["", "", ... , ""] #referecneNames for featureP
                            ]
    """
    text = refine(textWhole)
    readIndex = 0
    splitIndex = []
    referenceNames = []    
    
    while(readIndex < len(text)):
        if(text[readIndex][:25] == '// The class representing'):
           splitIndex.append(readIndex) 
        readIndex += 1
    splitIndex.append(len(text)) 
     
    #feature names, i.e. column names
    featureNames = getNames(text[splitIndex[0]:splitIndex[1]])
    
    #if a feature is continuous, its default referenceName is 'continuousFeature'
    referenceNames = ['continuousFeature'] * len(featureNames)
    #reference names for each feature
    for i in range(1,len(splitIndex)-1):
        ref = getNames(text[splitIndex[i]:splitIndex[i+1]])
        feature = text[splitIndex[i]][33:-1]
        if(feature in featureNames):
            featureIndex = featureNames.index(feature)
            referenceNames[featureIndex] = ref
    return [featureNames, referenceNames]
    
    
def getNames(text):
    ref = []
    start = text.index('    static final void fill(String[] sa) {\n')
    end = text.index('    }\n')
    for i in range(start+1, end):
        textLine = text[i]
        name = textLine[textLine.find('"')+1:-3]
        ref.append(name)
    return ref



######################### cleanRules ###################################

[kmin, kmax] = [-0.9900E+36, 0.9900E+36]


def cleanRules(Rules, feature, reference):
    Rules = convertTrick1(Rules, feature, reference) # mess up cases between categorical and continuous       
    Rules = restructureRules(Rules)   
    
    idx1 = 0
    while(idx1 < len(Rules)):
        rule1 = Rules[idx1]
        idx2 = 1
        while(idx1+idx2 < len(Rules)):
            rule2 = Rules[idx1+idx2]
            if(compareRules(rule1, rule2) == True):
                Rules.pop(idx1+idx2)
                idx2 -= 1
            idx2 += 1
        idx1 += 1      
    
    return Rules


def compareRules(rule1, rule2):
    # this is enough as a condition can's appear twice in a rule
    if(len(rule1) != len(rule2)):
        return False

    for condition in rule1:
        if(condition not in rule2):
            return False
    
    return True
            

def restructureRules(Rules):
    # retructure the condition on continuous feature to (type, feature, min, max)
    for rule in Rules:
        for i in range(len(rule)):
            rule[i] = restructureCondition(rule[i])
        
        idx1 = 0        
        while(idx1 < len(rule)):
            condition1 = rule[idx1]                        
            idx2 = 1
            while(idx1+idx2<len(rule)):
                condition2 = rule[idx1+idx2]
                if(condition1[:2] == condition2[:2] and condition1[0] == 0 ):
                    condition1[2] = max(condition1[2], condition2[2])    
                    condition1[3] = min(condition1[3], condition2[3])
                    rule.pop(idx1+idx2)
                    idx2 = idx2-1    
                idx2 = idx2 + 1
            idx1 = idx1+1
    return Rules


def restructureCondition(condition):
    if(condition[0] == 0 or condition[0] == -1):
        if(condition[3] == 0):
            condition[3] = condition[2]
            condition[2] = kmin
        else:
            condition[3] = kmax
            condition[0] = 0
    return condition
    
    
def convertTrick1(raw_Rules, feature, reference):
    for rule in raw_Rules:
        for condition in rule:
            if(condition[0]==0 and reference[condition[1]]!="continuousFeature"):
                cutoff = int(condition[2])
                setContain = [i for i in range(cutoff+1)]
                condition[2] = setContain
                condition[0] = 1
    return raw_Rules
    




















