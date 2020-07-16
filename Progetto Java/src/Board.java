import Execptions.*;
import java.util.*;
/*
 *   // Overview: Immutable ADT ,except thought his components
 *   //  The board contains the ownerid , his password and a list of categories
 *   //  TypicalElement:(owner,password,{category_0,...category_(dimW-1)})

 *   //AF:owner,password,category_box: t.c if(password)=passw && user==owner -> the user can write on the category_box
 *   // else if user!=owner -> user can read only if he respects the requirements of category

 * ------------------------------
 * Rep Invariant
 *     |
 *     V
 * *Contents and Category Whitelist must contain != NULL values , each txt added to a category content will be sorted by likes , category or txt (thought a lexicographical order)
 * Username , Password and Categories names are checked every time i use them.                                                      // CheckData checks it
 * The Board cant have 2 categories equal, a category whitelist cant have 2 username equal , a category cant have 2 data equals.
 */

public class Board <E extends Data> implements DataBoard<E> {

    private ArrayList<Category<E>> CategoryBox;           // Categories
    private String ownerid;                                    // Board Owner
    private String passw;                                      // Board Owner's Password


    //CONSTRUCTOR METHOD
    //Modifies:This
    //Effects:Initializes This
    //Requires: ownerid and passw are validated thought CheckData
    //Throws: InvalidDataException if ownerid or password are not valid.

    Board(String id, String password) throws DataNotValidExeption {
        CheckData.passwordchecker(password);
        CheckData.idchecker(id);
        this.ownerid = id;
        this.passw = password;
        CategoryBox = new ArrayList<>();
    }

    //Password Checker
    //Effect: checks if passwords match
    //Throws: throws WrongPassword if passwords don't match each other (checked)

    private void checkpass(String password) throws WrongPassword {
        if (!password.equals(this.passw)) throw new WrongPassword();
    }

    //Create Category
    //Modifies:This
    //Effects: Creates a new category
    //Requires: category and passw are validated thought CheckData
    //Throws: DuplicateException if there is category  (checked)
    //        WrongPassword if passwords does not match each other  (checked)
    //        InvalidDataException if category or password are not valid  (checked)

    @Override
    public void createCategory(String category, String password) throws DuplicateException, WrongPassword, DataNotValidExeption {
        CheckData.passwordchecker(password);
        CheckData.categorychecker(category);
        if (!password.equals(this.passw)) throw new WrongPassword();
        for (Category<E> actual : this.CategoryBox) {
            if (actual.getCategory().equals(category)) throw new DuplicateException("");
        }
        Category<E> tempcategory = new Category<>(category);
        this.CategoryBox.add(tempcategory);
        System.out.println("Categoria " + category + " Creata");
    }

    //Remove Category
    //Modifies:This
    //Effects: Removes a category that already exists
    //Requires: category and passw are validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other  (checked)
    //         InvalidDataException if category or password are not valid  (checked)
    //         DataNotFoundException if the category i want to delete is not in the Board  (checked)

    @Override
    public void removeCategory(String category, String password) throws WrongPassword, DataNotValidExeption, DataNotFoundException {
        CheckData.passwordchecker(password);
        CheckData.categorychecker(category);
        this.checkpass(passw);
        Category<E> temp = null;
        for (Category<E> actual : this.CategoryBox) {
            if (actual.getCategory().equals(category)) {
                temp = actual;                                       //PASSO L'INDIRIZZO A TEMP
            }
        }
        if (temp == null) throw new DataNotFoundException("The category " + category + " doesn't exist yet\n");
        this.CategoryBox.remove(temp);
    }

    //Adds a Friend
    //Modifies: category.whitelist
    //Effects: Adds user to a category whitelist
    //Requires: category and password are validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other (checked)
    //         DataNotValidException if category , password and friend are not valid  (checked)
    //         DuplicateException if friend is already in the whitelist  (checked)
    //         DataNotFoundException if the category does not exist (checked)


    @Override
    public void addFriend(String category, String password, String friend) throws WrongPassword, DuplicateException, DataNotValidExeption, DataNotFoundException {
        CheckData.categorychecker(category);
        CheckData.passwordchecker(password);
        CheckData.validateFriend(friend);
        this.checkpass(passw);
        Category<E> newCategory = null;
        for (Category<E> actual : this.CategoryBox) {
            if (actual.getCategory().equals(category)) {
                newCategory = actual;
            }
        }
        if (newCategory == null) throw new DataNotFoundException("The Category  " + category + " doesn't exist yet\n");
        ArrayList<String> whitelist = newCategory.getWhitelist();
        if (whitelist.contains(friend)) {
            this.CategoryBox.add(newCategory);
            throw new DuplicateException(friend + " already exists in " + category + "\n");
        }
        whitelist.add(friend);                                      //Adding new friend in the whitelist of chosen category
        newCategory.setWhitelist(whitelist);
        this.CategoryBox.add(newCategory);
    }


    //Removes a Friend
    //Modifies: category.whitelist
    //Effects: Adds user to a category whitelist
    //Requires: category , password and friends are validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other (checked)
    //         DataNotValidException if category , password and friend are not valid  (checked)
    //         NullPointerException friend is not the whitelist  (unchecked)
    //         DataNotFoundException if the category does not exist (checked)

    @Override
    public void removeFriend(String category, String password, String friend) throws NullPointerException, WrongPassword, DataNotFoundException, DataNotValidExeption {
        CheckData.categorychecker(category);
        CheckData.passwordchecker(password);
        CheckData.validateFriend(friend);
        this.checkpass(passw);
        Category<E> newCategory = null;
        for (Category<E> actual : this.CategoryBox) {
            if (actual.getCategory().equals(category)) {
                newCategory = actual;
            }
        }
        if (newCategory == null) throw new NullPointerException("The Category  " + category + " doesn't exist yet\n");
        this.CategoryBox.remove(newCategory);
        ArrayList<String> whitelist = newCategory.getWhitelist();
        if (!whitelist.contains(friend)) {
            this.CategoryBox.add(newCategory);
            throw new DataNotFoundException(friend + "doesn't exist yet");
        }
        whitelist.remove(friend);
        newCategory.setWhitelist(whitelist);
        this.CategoryBox.add(newCategory);
    }

    //Insert a Post
    //Modifies: category.content
    //Effects: Inserts a post in the category content
    //Requires: category and password are validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other (checked)
    //         DataNotValidException if category and password are not valid  (checked)
    //         NotAdminException the writer is not the owner of the Board  (checked)
    //         DataNotFoundException if the category does not exist  (checked)
    //         DuplicateException if data is already in the category content  (checked)

    @Override
    public boolean put(String password, E item, String category) throws WrongPassword, NotAdminException, DataNotFoundException, DuplicateException, DataNotValidExeption {
        CheckData.categorychecker(category);
        CheckData.passwordchecker(password);
        this.checkpass(password);
        if (item == null) throw new NullPointerException();
        if (!item.WhoWrote().equals(this.ownerid)) throw new NotAdminException("");

        Category<E> temp = null;
        for (Category<E> actual : this.CategoryBox) {
            if (actual.getCategory().equals(category)) {
                temp = actual;
            }
        }
        if (temp == null) throw new DataNotFoundException("");                           // Category not found -> DataNotFoundException
        this.CategoryBox.remove(temp);
        ArrayList<E> contents = temp.getContents();
        E item_clone = (E) item.clone();
        item_clone.setCategory(category);
        if (contents.contains(item)) {

            System.out.println("DuplicateException");
            this.CategoryBox.add(temp);
            throw new DuplicateException("");
        }
        contents.add(item_clone);
        temp.setContents(contents);
        this.CategoryBox.add(temp);
        return true;
    }

    //Insert a Like
    //Modifies: category.content
    //Effects: Inserts a post in the category content
    //Requires: friend is validated thought CheckData
    //Throws:  DataNotValidException if category and password are not valid  (checked)
    //         NotAdminException im not able to write or i did not found the item(checked)
    //         DuplicateException if a user already liked the item (checked)

    @Override
    public void insertLike(String friend, E item) throws DuplicateException, DataNotValidExeption, NotAdminException {
        CheckData.categorychecker(friend);
        if (item == null) throw new NullPointerException();
        E x = null;
        Category<E> updated = null;
        for (Category<E> actual : this.CategoryBox) {
            ArrayList<E> temp = actual.getContents();
            if (actual.getWhitelist().contains(friend) && temp.contains(item)) {
                updated = actual;
                temp.remove(item);
                x = (E) item.clone();
                try {
                    x.addLike(friend);
                } catch (DuplicateException e) {
                    temp.add(x);
                    throw e;
                }
                System.out.println("Just added like by " + friend + " to " + x.display());
                temp.add(x);
                actual.setContents(temp);
            }
        }
        if (x == null) throw new NotAdminException("");
    }

    //Gets a post
    //Modifies: category.content
    //Effects: gets a post from category content
    //Requires: friend is validated thought CheckData
    //Throws:  WrongPassword the password does not match with board password  (checked)
    //         DataNotFoundException the item is not contents  (checked)

    @Override
    public E get(String password, E item) throws WrongPassword, DataNotFoundException {
        this.checkpass(passw);
        if (item == null) throw new NullPointerException();
        E x = null;
        for (Category<E> actual : this.CategoryBox) {
            if (actual.getContents().contains(item)) x = item;
        }
        if (x == null) throw new DataNotFoundException(item.display() + "Not Found");
        return x;
    }

    //Removes a Post
    //Modifies: category.content
    //Effects: Removes a post in the category content
    //Requires: password is validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other (checked)
    //         DataNotFoundException if post is not in the content  (checked)
    @Override
    public E remove(String password, E item) throws WrongPassword, DataNotFoundException {
        this.checkpass(passw);
        if (item == null) throw new NullPointerException();
        E x = null;
        for (Category<E> actual : this.CategoryBox) {
            if (actual.getContents().contains(item)) {
                x = actual.getContents().get(actual.getContents().indexOf(item)); //SISTEMARE UN GET IN
                actual.getContents().remove(item);
                return x;
            }
        }
        throw new DataNotFoundException("");
    }

    //Get an iterator of a category
    //Effects: Returns all data of a chosen category
    //Requires: category and password are validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other (checked)
    //         DataNotValidException if category and password are not valid  (checked)
    //         DataNotFoundException if the category does not exist  (checked)

    @Override
    public ArrayList<E> getDataCategory(String passw, String category) throws WrongPassword, DataNotFoundException, DataNotValidExeption {
        CheckData.categorychecker(category);
        CheckData.passwordchecker(passw);
        this.checkpass(passw);
        Category<E> temp_category = null;
        for (Category<E> cat : this.CategoryBox) {
            if (cat.getCategory().equals(category)) {
                temp_category = cat;
            }
        }
        if (temp_category == null) throw new DataNotFoundException("This category does not exist " + category);
        ArrayList<E> temp = temp_category.getContents();
        return new ArrayList<E>(temp);
    }

    //Get an iterator of items
    //Effects: Returns all posts of a board
    //Requires: password is validated thought CheckData
    //Throws:  WrongPassword if passwords does not match each other (checked)
    //         DataNotValidException if password is not valid  (checked)

    @Override
    public Iterator<E> getIterator(String passw) throws WrongPassword, DataNotValidExeption {
        CheckData.passwordchecker(passw);
        this.checkpass(passw);
        ArrayList<E> all = new ArrayList<>();
        for (Category<E> actual : this.CategoryBox) {
            for (E e : actual.getContents()) {
                if (!all.contains(e))
                    all.add(e);
            }
        }
        Collections.sort(all);
        return all.iterator();
    }

    //Get an iterator of a category
    //Effects: Returns all data of a chosen category
    //Requires: friend is validated thought CheckData
    //Throws: DataNotValidException if friend is not valid  (checked)

    @Override
    public Iterator<E> getFriendIterator(String friend) throws DataNotValidExeption {
        CheckData.validateFriend(friend);
        ArrayList<E> all = new ArrayList<E>();
        for (Category<E> actual : this.CategoryBox) {
            if (actual.getWhitelist().contains(friend)) {
                for (E e : actual.getContents()) {
                    if (!all.contains(e))
                    all.add(e);
                }
            }
        }
        List<E> unmodifiableAll = Collections.unmodifiableList(all);
        return unmodifiableAll.iterator();
    }
}

