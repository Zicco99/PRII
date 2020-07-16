import Execptions.DataNotValidExeption;
/*
  I used this class to check every data input to isolate problems ,such as Divide et Impera , and
  to give a shape to the data type , i used Twitter style.
 */
public class CheckData {

    public static void passwordchecker(String p) throws DataNotValidExeption {
        if(p == null) throw new NullPointerException();
        if(p.length() == 0)  throw new DataNotValidExeption("Field Empty");
        if(p.length()<8 || p.length()>20) throw new DataNotValidExeption("Password Too long");
        if(p.contains(" ")) throw new DataNotValidExeption("");
    }

    public static void idchecker(String user) throws DataNotValidExeption {
        if(user == null) throw new NullPointerException();
        if(user.length() == 0) throw new DataNotValidExeption("Field Empty");
        if(user.length()<5 || user.length()>20) throw new DataNotValidExeption("");
        if(user.contains(" ")) throw new DataNotValidExeption("");
    }

    public static void textchecker(String text) throws DataNotValidExeption{
        if(text == null) throw new NullPointerException();
        if(text.length() == 0)  throw new DataNotValidExeption("Text field is empty");
        if(text.length() > 160) throw new DataNotValidExeption("Text field is greater than 160 chars");      // Twitter Style: Max 160 chars
    }

    public static void categorychecker(String cat) throws DataNotValidExeption {
        if (cat == null) throw new NullPointerException();
        if (cat.length() == 0) throw new DataNotValidExeption("Category field is empty");
        if (cat.length() >30) throw new DataNotValidExeption("Category field is greater than 30 chars");
    }

    public static void validateFriend(String friend) throws DataNotValidExeption {
        if(friend == null) throw new NullPointerException();
        if(friend.length() == 0)  throw new DataNotValidExeption("Field Empty");
        if(friend.length()<5 || friend.length()>20) throw new DataNotValidExeption("Username Too long");
        if(friend.contains(" ")) throw new DataNotValidExeption("");
    }
}
