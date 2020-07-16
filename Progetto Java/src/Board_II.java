import Execptions.*;

import java.util.*;
/*
 *   // Overview: Immutable ADT ,except thought his components
 *   // The board contains a set of contents and a whitelist , the owner id and his password
 *   // TypicalElement:(owner,password,{(category_0,data_0)...,(category_(dim-1),data_(dim-1)}, {(category_0,{friend_0,....,friend_(n-1)})..,(category_0,{friend_0,....,friend_(n-1)})
 *   // First HashMap contains a set (category,data) where the key is the category, second one contains a set (category,whitelist) where the key is the category
 *   // AF:owner,password: t.c if(password)==password && user==owner -> the user can write on both HashMap
 *   // else if user!=owner -> user can read only

 * ------------------------------
 * Rep Invariant
 *     |
 *     V
 * *Contents and Category Whitelist must contain != NULL values , each txt added to a category content will be sorted by likes , category or txt (thought a lexicographical order)
 * Username , Password and Categories names are checked every time i use them.                                                      // CheckData checks it
 * The Board cant have 2 categories equal, a category whitelist cant have 2 username equal , a category cant have 2 data equals.
 * HashMap won't contain null data cause i check them with CheckData.java
 *  */
public class Board_II <E extends Data> implements DataBoard<E> {
        private HashMap<String, TreeSet<E>> contents;
        private HashMap<String, HashSet<String>> whitelist;
        private String id;
        private String password;

    //CONSTRUCTOR METHOD
    //Modifies:This
    //Effects:Initializes This
    //Requires: ownerid and passw are validated thought CheckData
    //Throws: InvalidDataException if ownerid or password are not valid.

        public Board_II(String id, String password) throws DataNotValidExeption {
            CheckData.idchecker(id);
            CheckData.passwordchecker(password);
            this.contents = new HashMap<>();
            this.whitelist = new HashMap<>();
            this.id = id;
            this.password = password;
        }
        public String getOwner() {
            return this.id;
        }

    //Password Checker
    //Effect: checks if passwords match
    //Throws: throws WrongPassword if passwords don't match each other (checked)

    public void checkpass(String p) throws WrongPassword, DataNotValidExeption {
            CheckData.passwordchecker(p);
            if(!this.password.equals(p)) throw new WrongPassword("");
        }

    //Create Category
    //Modifies:This
    //Effects: Creates a new category
    //Requires: category and passw are validated thought CheckData
    //Throws: DuplicateException if category already exists in contents or in whitelist (checked)
    //        WrongPassword if passwords does not match each other  (checked)
    //        InvalidDataException if category or password are not valid  (checked)
        @Override
        public void createCategory(String category, String password) throws DataNotValidExeption, WrongPassword, DuplicateException {
            CheckData.categorychecker(category);
            this.checkpass(password);
            if (this.contents.containsKey(category)) throw new DuplicateException(category);     //Category has a duplicate in content HashMap
            if (this.whitelist.containsKey(category)) throw new DuplicateException(category);    //Category has a duplicate in whitelist HashMap
            this.contents.put(category, new TreeSet<>());
            this.whitelist.put(category, new HashSet<>());
        }

    //Remove Category
    //Modifies:This
    //Effects: Removes a category that already exists
    //Requires: category and passw are validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other  (checked)
    //         InvalidDataException if category or password are not valid  (checked)
    //         DataNotFoundException if category does not exist in both HashMap (checked)

        @Override
        public void removeCategory(String category, String passw) throws WrongPassword, DataNotValidExeption, DataNotFoundException {
            CheckData.categorychecker(category);
            this.checkpass(passw);
            if(this.contents.remove(category) == null) throw new DataNotFoundException("");     //Category has a duplicate in content HashMap
            if(this.whitelist.remove(category) == null) throw new DataNotFoundException("");    //Category has a duplicate in whitelist HashMap
        }

    // Adds a Friend
    //Modifies: this.whitelist
    //Effects: Adds user to whitelist
    //Requires: category and password are validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other (checked)
    //         DataNotValidException if category , password and friend are not valid  (checked)
    //         DuplicateException if friend is already in the whitelist found thought key=category  (checked)
    //         DataNotFoundException if the category does not exist in both HashMap(checked)

        @Override
        public void addFriend(String category, String passw, String friend) throws DataNotFoundException, WrongPassword, DataNotValidExeption, DuplicateException {
            CheckData.categorychecker(category);
            CheckData.validateFriend(friend);
            this.checkpass(passw);
            if(!this.whitelist.containsKey(category)) throw new DataNotFoundException("");
            HashSet<String> actual_whitelist = this.whitelist.get(category);
            if(actual_whitelist.contains(friend)) throw new DuplicateException("");
            actual_whitelist.add(friend);
        }
    //Removes a Friend
    //Modifies: this.whitelist
    //Effects: Adds user to a category whitelist
    //Requires: category , password and friends are validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other (checked)
    //         DataNotValidException if category , password and friend are not valid (checked)
    //         DataNotFoundException if the category does not exist in whitelist (checked)

        @Override
        public void removeFriend(String category, String passw, String friend) throws WrongPassword, DataNotValidExeption, DataNotFoundException {
            CheckData.categorychecker(category);
            CheckData.validateFriend(friend);
            this.checkpass(passw);
            if(!this.whitelist.containsKey(category)) throw new DataNotFoundException("");
            HashSet<String> actual_whitelist = this.whitelist.get(category);
            if(!actual_whitelist.contains(friend)) throw new DataNotFoundException("");
            actual_whitelist.remove(friend);
        }

    //Insert a Post
    //Modifies: this.content
    //Effects: Inserts a post in the category content
    //Requires: category and password are validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other (checked)
    //         DataNotValidException if category or password are not valid  (checked)
    //         NotAdminException the writer is not the owner of the Board  (checked)
    //         DataNotFoundException if the category does not exist in both HashMap  (checked)
    //         DuplicateException if data is already in content (checked)

        @Override
        public boolean put(String passw, E dato, String category) throws WrongPassword, DataNotValidExeption, DataNotFoundException, DuplicateException, NotAdminException {
            if(dato == null) throw new NullPointerException();
            CheckData.categorychecker(category);
            this.checkpass(passw);
            if(!this.contents.containsKey(category)) throw new DataNotFoundException("");
            TreeSet<E> actual_contents = this.contents.get(category);
            if(!dato.getAuthor().equals(this.getOwner())) throw new NotAdminException("");

            E copy = (E)dato.clone();
            copy.setCategory(category);
            if(actual_contents.contains(copy)) throw new DuplicateException("");
            actual_contents.add(copy);
            return true;
        }

    //Gets a post
    //Modifies: category.content
    //Effects: gets a post from category content , Returns the post
    //Requires: friend is validated thought CheckData
    //Throws:  WrongPassword the password does not match with board password  (checked)
    //         DataNotFoundException the item is not in contents  (checked)
    //         DataNotValidException if password is not valid (checked)

        @Override
        public E get(String password, E item) throws DataNotFoundException, WrongPassword, DataNotValidExeption {
            this.checkpass(password);
            if(item == null) throw new NullPointerException();
            E x = null;
            for(TreeSet<E> actual : this.contents.values()) {
                if(actual.contains(item)) x = item;
            }
            if(x == null) throw new DataNotFoundException("");
            return x;
        }

    //Removes a Post
    //Modifies: this.content
    //Effects: Removes a post in the category content, returns the post removed
    //Requires: password is validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other (checked)
    //         DataNotFoundException if post is not in the content  (checked)
    //         DataNotValidException if password is not valid  (checked)

        @Override
        public E remove(String passw, E item) throws DataNotFoundException, WrongPassword, DataNotValidExeption {
            this.checkpass(passw);
            if(item == null) throw new NullPointerException();
            E x = null;
            for(TreeSet<E> t : this.contents.values()) {
                if(t.remove(item)) x = item;
            }
            if(x == null) throw new DataNotFoundException(item.display());
            return x;
        }

    //Get an iterator of category
    //Effects: Returns all posts of a board
    //Requires: password is validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other (checked)
    //         DataNotValidException if password is not valid  (checked)
    //         DataNotFoundException category does not exist in contents (checked)

        @Override
        public ArrayList<E> getDataCategory(String passw, String category) throws DataNotFoundException, DataNotValidExeption, WrongPassword {
            this.checkpass(passw);
            CheckData.categorychecker(category);
            if(!this.contents.containsKey(category)) throw new DataNotFoundException("");
            TreeSet<E> actual_contents = this.contents.get(category);
            return new ArrayList<E>(actual_contents);
        }

    //Insert a Like
    //Modifies: this.content
    //Effects: Inserts a post in the category content
    //Requires: friend is validated thought CheckData
    //Throws:  DataNotValidException if category and password are not valid  (checked)
    //         NotAdminException im not able to write or i did not found the item(checked)
    //         DuplicateException if a user already liked the item (checked)

        @Override
        public void insertLike(String friend, E item) throws DuplicateException, DataNotValidExeption, NotAdminException {
            if(item == null) throw new NullPointerException();
            E x = null;
            for(String category : this.contents.keySet()) {
                TreeSet<E> t = this.contents.get(category);
                if(this.whitelist.get(category) == null) throw new NullPointerException("");
                if(this.whitelist.get(category).contains(friend)) {
                    if(t.contains(item)) {
                        t.remove(item);
                        x = (E) item.clone();
                        try {
                            x.addLike(friend);
                        } catch(DuplicateException e) {
                                t.add(x);
                                throw e;
                        }
                            t.add(x);
                        }
                    }
                }
                if(x == null) throw new NotAdminException("");
            }

    //Get an iterator of items
    //Effects: Returns all posts of a board
    //Requires: password is validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other (checked)
    //         DataNotValidException if password is not valid  (checked)

            @Override
            public Iterator<E> getIterator(String passw) throws WrongPassword, DataNotValidExeption {
                this.checkpass(passw);
                TreeSet <E> all = new TreeSet<E>();
                for(TreeSet<E> t : this.contents.values()) {
                    all.addAll(t);
                }
                SortedSet<E> unmodifiableAll = Collections.unmodifiableSortedSet(all);
                return unmodifiableAll.iterator();
            }

    //Get an iterator of a category
    //Effects: Returns all data of a chosen category
    //Requires: friend is validated thought CheckData
    //Throws: DataNotValidException if friend is not valid  (checked)

            @Override
            public Iterator<E> getFriendIterator(String friend) throws DataNotValidExeption {
                CheckData.validateFriend(friend);
                ArrayList <E> all = new ArrayList<E>();
                for(String cat : this.contents.keySet()) {
                    if(this.whitelist.get(cat).contains(friend)) {
                        all.addAll(this.contents.get(cat));
                    }
                }
                List<E> unmodifiableAll = Collections.unmodifiableList(all);
                return unmodifiableAll.iterator();
        }
}
