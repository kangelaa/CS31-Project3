//
//  main.cpp
//  Project 3
//
//  Created by Angela Kan on 11/3/20.
//

#include <iostream>
#include <string>
#include <cctype>
#include <cassert>
using namespace std;

bool isSyntacticallyCorrect(string pollData);
int tallyVotes(string pollData, char party, int& voteTally);
bool isValidUppercaseStateCode(string stateCode);
bool checkCorrectStateCode(string pollData, int& pos);

//checks test cases
int main() {
    assert(isSyntacticallyCorrect(""));
    assert(isSyntacticallyCorrect("5CAD"));
    assert(isSyntacticallyCorrect("55CAD"));
    assert(!isSyntacticallyCorrect("55C"));
    assert(!isSyntacticallyCorrect("55"));
    assert(!isSyntacticallyCorrect("5"));
    assert(isSyntacticallyCorrect("00CAD"));
    assert(isSyntacticallyCorrect("0cAd"));
    assert(!isSyntacticallyCorrect("!"));
    assert(!isSyntacticallyCorrect("4ca0"));
    assert(!isSyntacticallyCorrect("433cad"));
    assert(!isSyntacticallyCorrect("43cadd"));
    assert(!isSyntacticallyCorrect("43ca4d"));
    assert(isSyntacticallyCorrect("38TXR55CAD6Msr29nYd06UTL"));
    assert(!isSyntacticallyCorrect("38TXR:55CAD"));
    assert(!isSyntacticallyCorrect("38TXR 55CAD"));
    int votes = -999;
    assert(tallyVotes("38TXR55CAD6Msr29nYd06UTL", 'd', votes) == 0 && votes == 84);
    votes = -999;    // so we can detect whether tallyVotes sets votes
    assert(tallyVotes("38TXR55CAD", '%', votes) == 2  &&  votes == -999);
    votes = -999;
    assert(tallyVotes("55C", 'R', votes) == 1 && votes == -999);
    assert(tallyVotes("55C", '%', votes) == 1 && votes == -999);
    assert(tallyVotes("", '@', votes) == 2 && votes == -999);
    assert(tallyVotes("", 'R', votes) == 0 && votes == 0);
    assert(tallyVotes("38TXR", 'r', votes) == 0 && votes == 38);
    //assert(tallyVotes("00TXR", 'r', votes) == 3 && votes == -999);
    //assert(tallyVotes("0TXR", 'r', votes) == 3 && votes == -999);
    //assert(tallyVotes("00TXR", 'X', votes) == 3 && votes == -999);
    assert(tallyVotes("38TXR55CAD6Msr0nYd06UTL", 'd', votes) == 3 && votes == -999);

    cout << "All tests succeeded" << endl;
}

//checks for syntactically correct poll data string
bool isSyntacticallyCorrect(string pollData){
    if (pollData == ""){        //checks for empty string case, returns true if yes
        return true;
    } else {
        int k = 0;
        while (k != pollData.size()){
            if (isdigit(pollData.at(k))){           //if char at k is digit, then will check for state code
                if (!checkCorrectStateCode(pollData, k)){
                    return false;
                } else if (isalpha(pollData.at(k))){
                    k++;
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
        return true;
    }
}

int tallyVotes(string pollData, char party, int& voteTally){
    if (!isSyntacticallyCorrect(pollData)){
        return 1;
    } else if (!isalpha(party)){
        return 2;
    } else {
        string uppercasePollData = "";
        for (int i = 0; i != pollData.size(); i++){
            uppercasePollData += toupper(pollData.at(i));
        }
        party = toupper(party);
        int votes = 0;
        while (uppercasePollData.size()>0){
            int pos = 0;
            char a = 0;
            char b = 0;
            int aa = 0;
            int bb = 0;
            if (isdigit(uppercasePollData.at(pos+1))){
                a = uppercasePollData.at(pos);
                aa = a - '0';
                b = uppercasePollData.at(pos+1);
                bb = b - '0';
                pos++;
            } else if (isdigit(uppercasePollData.at(pos))){
                b = uppercasePollData.at(pos);
                bb = b - '0';
            }
            if (aa * 10 + bb == 0){
                return 3;
            } else {
                if (uppercasePollData.at(pos+3) == party){
                    votes += aa * 10 + bb;
                }
            }
            uppercasePollData = uppercasePollData.substr(pos + 4, uppercasePollData.size() - (pos + 4));
        }
        voteTally = votes;
        return 0;
    }
}

bool isValidUppercaseStateCode(string stateCode){
    const string codes =
        "AL.AK.AZ.AR.CA.CO.CT.DE.DC.FL.GA.HI.ID.IL.IN.IA.KS."
        "KY.LA.ME.MD.MA.MI.MN.MO.MS.MT.NE.NV.NH.NJ.NM.NY.NC."
        "ND.OH.OK.OR.PA.RI.SC.SD.TN.TX.UT.VT.VA.WA.WV.WI.WY";
    
    return (stateCode.size() == 2  &&
            stateCode.find('.') == string::npos  &&  // no '.' in stateCode
            codes.find(stateCode) != string::npos);  // match found
}

bool checkCorrectStateCode(string pollData, int& pos){
    string numOfVotes;
    int count = 0;
    for (numOfVotes = ""; isdigit(pollData.at(pos)); pos++){        //checks for numbers after digit, adds to numOfVotes and leaves off on next (letter) position
        numOfVotes += pollData.at(pos);
        count++;
        //section about maxing num of votes to 2 digit number
        if (count >= 3){
            return false;
        }
        if (pos == pollData.size() - 1){
            return false;
        }
    }
    
    string uppercasePollData = "";
    for (int i = 0; i < pollData.size(); i++){
        uppercasePollData += toupper(pollData.at(i));   //changes poll string to all upppercase before comparing w/ state codes
    }
    
    string stateCode;
    for (int i = pos; pos < i + 2; pos++){
        stateCode += uppercasePollData.at(pos);
        // test for case where there is one letter state code/no state code (out of bounds)
        if (pos == pollData.size() - 1){
            return false;
        }
    }
    
    if (!isValidUppercaseStateCode(stateCode)){
        return false;
    } else {
        return true;
    }
}



