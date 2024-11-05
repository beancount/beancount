from interop import Address, ContactInfo, Person

def test_basic_usage():
    # Create test objects
    address = Address(street="123 Main St", city="Springfield", country="USA")
    contact = ContactInfo(email="john@example.com", phone="555-0123")
    person = Person(name="John Doe", age=30, address=address, contact=contact)
    
    # Test getters
    assert person.name == "John Doe"
    assert person.age == 30
    assert person.address.street == "123 Main St"
    assert person.contact.email == "john@example.com"
    
    # Test setters
    person.age = 31
    person.address.street = "456 Oak Ave"
    assert person.age == 31
    assert person.address.street == "456 Oak Ave"
    
    # Test friend functionality
    friend = Person(
        name="Jane Smith",
        age=28,
        address=Address(street="789 Pine St", city="Riverside", country="USA"),
        contact=ContactInfo(email="jane@example.com", phone="555-0456")
    )
    person.add_friend(friend)
    assert person.get_friend_count() == 1

def test_nested_mutation():
    address = Address(street="123 Main St", city="Springfield", country="USA")
    contact = ContactInfo(email="john@example.com", phone="555-0123")
    person = Person(name="John Doe", age=30, address=address, contact=contact)
    
    # Test nested mutations
    person.address.street = "New Street"
    person.contact.email = "new.email@example.com"
    
    assert person.address.street == "New Street"
    assert person.contact.email == "new.email@example.com"
