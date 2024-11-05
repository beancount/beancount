#!/usr/bin/env python3

from interop import Address, ContactInfo, Person

# Create nested objects
address = Address(street="123 Main St", city="Springfield", country="USA")
contact = ContactInfo(email="john@example.com", phone="555-0123")

# Create a person with nested structs
person = Person(name="John Doe", age=30, address=address, contact=contact)

# All fields are mutable
person.age = 31
person.address.street = "456 Oak Ave"

# Create another person and add as friend
friend = Person(
    name="Jane Smith",
    age=28,
    address=Address(street="789 Pine St", city="Riverside", country="USA"),
    contact=ContactInfo(email="jane@example.com", phone="555-0456"),
)

person.add_friend(friend)
print(person.get_friend_count())  # Prints: 1
print(person)
