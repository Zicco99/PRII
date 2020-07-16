import Execptions.*;

import java.util.ArrayList;
import java.util.Iterator;

public interface DataBoard <E extends Data>{

        //Adds a category if verified thought password
        public void createCategory(String category,String password) throws DuplicateException, WrongPassword, DataNotValidExeption;

        //Removes a category if verified thought password
        public void removeCategory(String category,String password) throws WrongPassword, DataNotFoundException, DataNotValidExeption;

        //Adds a friend to the whitelist of viewers if verified thought password
        public void addFriend(String category,String password,String Friend) throws WrongPassword, DataNotFoundException, DuplicateException, DataNotValidExeption;

        //Removes a friend to the whitelist of viewers if verified thought password
        public void removeFriend(String category,String password,String Friend) throws WrongPassword, DataNotFoundException, DataNotValidExeption;

        //Add an element to the chosen category if verified thought password
        public boolean put (String password,E item,String category) throws WrongPassword, NotAdminException, DataNotFoundException, DuplicateException, DataNotValidExeption;

        //Returns an element copy if verified thought password
        public E get(String password, E item) throws WrongPassword, DataNotFoundException, DataNotValidExeption;

        //Removes an element copy if verified thought password
        public E remove(String password, E item) throws WrongPassword, DataNotFoundException, DataNotValidExeption;

        //Add a like to the chosen data
        public void insertLike(String friend,E data) throws NotAdminException, Exception, DuplicateException, DataNotFoundException, UserNotValidExeption;

        //Returns the list of data of a chosen category
        public ArrayList<E> getDataCategory(String passw, String category) throws WrongPassword, DataNotFoundException, DataNotValidExeption;

        //Creates a data iterator sorted by likes
        public Iterator<E> getIterator(String passw) throws WrongPassword, DataNotValidExeption;

        //Creates a shared data iterator
        public Iterator<E> getFriendIterator(String friend) throws DataNotValidExeption;

}
