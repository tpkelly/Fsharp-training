module ExerciseOptional

    open Exercise
//Part Two: Use your functions to achieve the objective below

    type DomainExtension = 
        |CoUk
        |Com

    type Contact = {
        FirstName : string
        LastName : string
        Email : string
        EmailExtension : DomainExtension
    }

    type ContactStats = {
        LongestNameContact : Contact
        LongestEmailContact : Contact
        CountContactsWithCoUk : int
        CountContactsWithCom : int
        TotalNumberOfContacts : int
    }

    let InitStatsWithContact contact = {LongestNameContact = contact; LongestEmailContact = contact; CountContactsWithCoUk = 0; CountContactsWithCom = 0; TotalNumberOfContacts = 0}

    let FSharpStudyGroup = "Andrew Lee <alee@scottlogic.co.uk>; Chris Smith <csmith@scottlogic.com>; Ian Lovell <ilovell@scottlogic.co.uk>; Jason Ebbin <JEbbin@scottlogic.com>; John Dawes <JDawes@scottlogic.com>; Matthew Dunsdon <MDunsdon@scottlogic.co.uk>; Nicholas Soper <NSoper@scottlogic.com>; Richard Doyle <rdoyle@scottlogic.co.uk>; Sean O’Neill <SONeill@scottlogic.com>; Stephen Ford <SFord@scottlogic.co.uk>; Thomas Kelly <tkelly@scottlogic.com>; Tyler Ferguson <TFerguson@scottlogic.com>"

    // Take a string and split it by ';' return a list of the split values
    let splitStringBySemiColons (semiColonSepString : string) =
        semiColonSepString.Split ';'
        |> Array.toList
        |> mapList (fun x -> x.Trim())

    //Take a contact string, E.g. Andrew Lee <alee@scottlogic.co.uk> and parse to the Contact Type
    //let parseContact (contact:string) = {FirstName = ""; LastName=""; Email = ""; EmailExtension = CoUk}

    //Note: This was originally going to be apart of the exercise, but we decided to take it out so we don't have a discussion about parsing...
    //I've given a really bad implementation as bait though, so if you feel like it, come back and write a nice one!
    //Otherwise, we may use this as a point as discussion in the future. 
    let parseContact (contact:string) = 
        let split = contact.Trim().Split()
        let firstName = split.[0].Trim()
        let lastName = split.[1].Trim()
        let email = split.[2].Trim('<','>')
        let emailExtension = 
            match email.Contains "co.uk" with
            | true -> CoUk
            | false -> Com
        {FirstName = firstName; LastName=lastName; Email = email; EmailExtension = emailExtension}

    // Return "They have a UK domain Extension, YAY!" if the extension is .co.uk. Return "Why do they have a COM domain Extension? meh." if the extension is .com
    let getExtensionPhrase (extension:DomainExtension) =
        match extension with
        | CoUk -> "They have a UK domain Extension, YAY!"
        | Com -> "Why do they have a COM domain Extension? meh."

    (*
        Return one of the following strings for each Contact
        "{FirstName} {LastName} has an email of {Email}. They have a UK domain Extension, YAY!."
        or 
        "{FirstName} {LastName} has an email of {Email}. Why do they have a COM domain Extension? meh."
    *)
    let getContactString (contact:Contact) =
        contact.FirstName + " " + contact.LastName + " has an email of " + contact.Email + ". " + getExtensionPhrase(contact.EmailExtension)

    // create a function that calls ParseContact then calls GetContactString with the result of ParseContact
    let transformContactString : (string -> string) = (fun x -> parseContact x |> getContactString)
        
    
    (*
        The following should print:
        FirstName LastName has an email of Email. They have a UK domain Extension, YAY!. 
        or 
        FirstName LastName has an email of Email. Why do they have a COM domain Extension? meh. 

        on a new line for each Contact in the FSharpStudyGroup String.

        HINT
        The you should be able to do this function only with the functions created above, 
        
    *)
    let rec prettyPrintFSharpGroupRecords (recordsList : string list) =
        match recordsList with
        | x::xs -> System.Console.WriteLine (transformContactString x)
                   prettyPrintFSharpGroupRecords xs
        | [] -> ()
        


    //TODO write an accumulator function which will take the current stats, a new contact, and return the updated statistics. 
    //This will be used in PrintRecordsStats
    let StatsFolder (acc: ContactStats) (elem: Contact) : ContactStats =
        let longestEmail = 
            if (elem.Email.Length > acc.LongestEmailContact.Email.Length)
            then elem
            else acc.LongestEmailContact
        let longestName =
            if (elem.FirstName.Length + elem.LastName.Length > acc.LongestNameContact.FirstName.Length + acc.LongestNameContact.LastName.Length)
            then elem
            else acc.LongestNameContact

        let UKCount = if (elem.EmailExtension = CoUk) then acc.CountContactsWithCoUk+1 else acc.CountContactsWithCoUk
        let comCount = if (elem.EmailExtension = Com) then acc.CountContactsWithCom+1 else acc.CountContactsWithCom
                
        {LongestNameContact = longestName;
        LongestEmailContact = longestEmail;
        CountContactsWithCoUk = UKCount;
        CountContactsWithCom = comCount;
        TotalNumberOfContacts = acc.TotalNumberOfContacts + 1}

    (*
        The following should print the following lines, based on the records list: 
        "{Contact} has the longest full name"
        "{Contact} has the longest email"
        "There are {number} contacts with an email with extension .co.uk"
        "There are {number} contacts with an email with extension .com"
        "There are {number} contacts in total"

        HINTS
        Use the functions you have created above and printfn "%A"
        Modify the ContactStats type above to help pretty print it. See : http://stackoverflow.com/a/13537020 and https://msdn.microsoft.com/en-us/library/ee370560.aspx

    *)
    let printRecordsStats (recordsList : string list) =
        let contactsList = mapList parseContact recordsList
        let acc = InitStatsWithContact contactsList.Head
        let stats = foldList StatsFolder acc contactsList

        System.Console.WriteLine(stats.LongestNameContact.FirstName + " " + stats.LongestNameContact.LastName + " has the longest full name")
        System.Console.WriteLine(stats.LongestNameContact.FirstName + " " + stats.LongestNameContact.LastName + " has the longest email")
        System.Console.WriteLine("There are " + stats.CountContactsWithCoUk.ToString() + " contacts with an email extension .co.uk")
        System.Console.WriteLine("There are " + stats.CountContactsWithCom.ToString() + " contacts with an email extension .com")
        System.Console.WriteLine("There are " + stats.TotalNumberOfContacts.ToString() + " contacts in total")

    let executeExercise() = 
        let recordsList = splitStringBySemiColons FSharpStudyGroup
        prettyPrintFSharpGroupRecords recordsList
        printRecordsStats recordsList

