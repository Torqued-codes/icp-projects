import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Debug "mo:base/Debug";
import HashMap "mo:base/HashMap";
import Hash "mo:base/Hash";
import Int "mo:base/Int";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Nat8 "mo:base/Nat8";
import Nat32 "mo:base/Nat32";
import Nat64 "mo:base/Nat64";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Time "mo:base/Time";
import TrieMap "mo:base/TrieMap";
import Buffer "mo:base/Buffer";
import Result "mo:base/Result";
import Float "mo:base/Float";

actor TicketingSystem {

    // === Types ===
    
    // Event Type
    type Event = {
        eventId: Nat;
        name: Text;
        date: Time.Time;
        venue: Text;
        price: Nat; // In ICP e8s (10^8 e8s = 1 ICP)
        totalTickets: Nat;
        availableTickets: Nat;
        organizer: Principal;
        isActive: Bool;
        description: Text;
        imageUrl: ?Text;
    };

    // Ticket Type (NFT)
    type Ticket = {
        tokenId: Nat;
        eventId: Nat;
        owner: Principal;
        originalPrice: Nat;
        currentPrice: Nat;
        purchaseHistory: [TicketTransfer];
        isValid: Bool;
        metadata: ?TicketMetadata;
    };

    type TicketMetadata = {
        eventName: Text;
        ticketClass: Text; // e.g., "VIP", "Standard"
        seatInfo: ?Text;
        purchaseDate: Time.Time;
    };

    type TicketTransfer = {
        from: Principal;
        to: Principal;
        price: Nat;
        timestamp: Time.Time;
    };

    // For NFT implementation (DIP721 compliance)
    type TokenMetadata = {
        tokenId: Nat;
        owner: Principal;
        metadata: ?Blob;
        properties: [(Text, Text)];
        isApproved: Bool;
    };

    // Error Types
    type Error = {
        #NotFound;
        #AlreadyExists;
        #NotAuthorized;
        #InvalidOperation;
        #InsufficientFunds;
        #SoldOut;
        #LimitExceeded;
    };

    // Role Types
    type Role = {
        #Admin;
        #Organizer;
        #User;
    };

    // Payment Types
    type PaymentStatus = {
        #Pending;
        #Completed;
        #Failed;
        #Refunded;
    };

    type Payment = {
        paymentId: Text;
        payer: Principal;
        recipient: Principal;
        amount: Nat;
        status: PaymentStatus;
        timestamp: Time.Time;
    };

    // Response Types
    type EventResponse = Result.Result<Event, Error>;
    type TicketResponse = Result.Result<Ticket, Error>;
    type EventsResponse = Result.Result<[Event], Error>;
    type TicketsResponse = Result.Result<[Ticket], Error>;
    type BoolResponse = Result.Result<Bool, Error>;
    type NatResponse = Result.Result<Nat, Error>;

    // === State Variables ===
    
    stable var nextEventId : Nat = 1;
    stable var nextTicketId : Nat = 1;
    
    // Principal state maps
    private var events = HashMap.HashMap<Nat, Event>(0, Nat.equal, Hash.hash);
    private var tickets = HashMap.HashMap<Nat, Ticket>(0, Nat.equal, Hash.hash);
    private var userRoles = HashMap.HashMap<Principal, Role>(0, Principal.equal, Principal.hash);
    private var userTickets = HashMap.HashMap<Principal, [Nat]>(0, Principal.equal, Principal.hash);
    private var organizerEvents = HashMap.HashMap<Principal, [Nat]>(0, Principal.equal, Principal.hash);
    private var payments = HashMap.HashMap<Text, Payment>(0, Text.equal, Text.hash);

    // Constants
    let MAX_RESALE_MULTIPLIER : Float = 1.2;
    let PLATFORM_FEE_PERCENTAGE : Nat = 2; // 2% fee
    
    // System management
    private stable var owner : Principal = Principal.fromText("yy5op-5es54-ps3vf-sjq7w-l4fc2-c4ltv-rszdn-fqjuz-wqrrt-d3er5-aae"); // Will be updated in init
    
    // === System Initialization ===
    
    public shared(msg) func initialize() : async () {
        assert(owner == Principal.fromText("yy5op-5es54-ps3vf-sjq7w-l4fc2-c4ltv-rszdn-fqjuz-wqrrt-d3er5-aae") or msg.caller == owner);
        owner := msg.caller;
        userRoles.put(msg.caller, #Admin);
    };

    // === Event Management Functions ===
    
    // Create a new event
    public shared(msg) func createEvent(
        name: Text,
        date: Time.Time,
        venue: Text, 
        price: Nat,
        totalTickets: Nat,
        description: Text,
        imageUrl: ?Text
    ) : async EventResponse {
        // Verify caller is an organizer or admin
        let callerRole = Option.get(userRoles.get(msg.caller), #User);
        if (callerRole != #Organizer and callerRole != #Admin) {
            return #err(#NotAuthorized);
        };
        
        // Validate inputs
        if (Text.size(name) == 0) {
            return #err(#InvalidOperation);
        };
        
        if (date < Time.now()) {
            return #err(#InvalidOperation);
        };
        
        // Create event
        let eventId = nextEventId;
        nextEventId += 1;
        
        let newEvent : Event = {
            eventId = eventId;
            name = name;
            date = date;
            venue = venue;
            price = price;
            totalTickets = totalTickets;
            availableTickets = totalTickets;
            organizer = msg.caller;
            isActive = true;
            description = description;
            imageUrl = imageUrl;
        };
        
        events.put(eventId, newEvent);
        
        // Add event to organizer's events
        let organizerEventsList = Option.get(organizerEvents.get(msg.caller), []);
        organizerEvents.put(msg.caller, Array.append(organizerEventsList, [eventId]));
        
        #ok(newEvent);
    };
    
    // Get event details by ID
    public query func getEvent(eventId: Nat) : async EventResponse {
        switch(events.get(eventId)) {
            case (?event) { #ok(event) };
            case null { #err(#NotFound) };
        };
    };
    
    // Get all events
    public query func getAllEvents() : async EventsResponse {
        let allEvents = Iter.toArray(events.vals());
        #ok(allEvents);
    };

    // Get all active events (not yet occurred)
    public query func getActiveEvents() : async EventsResponse {
        let currentTime = Time.now();
        let eventsBuffer = Buffer.Buffer<Event>(0);
        
        for (event in events.vals()) {
            if (event.date > currentTime and event.isActive and event.availableTickets > 0) {
                eventsBuffer.add(event);
            };
        };
        
        #ok(Buffer.toArray(eventsBuffer));
    };
    
    // Update event details (only organizer or admin)
    public shared(msg) func updateEvent(
        eventId: Nat,
        name: ?Text,
        date: ?Time.Time,
        venue: ?Text,
        price: ?Nat,
        totalTickets: ?Nat,
        isActive: ?Bool,
        description: ?Text,
        imageUrl: ?Text
    ) : async EventResponse {
        switch(events.get(eventId)) {
            case (?event) {
                // Verify caller is the organizer or admin
                if (msg.caller != event.organizer and 
                    Option.get(userRoles.get(msg.caller), #User) != #Admin) {
                    return #err(#NotAuthorized);
                };
                
                // Check if additional tickets are being added
                let newTotalTickets = Option.get(totalTickets, event.totalTickets);
                let additionalTickets = if (newTotalTickets > event.totalTickets) {
                    newTotalTickets - event.totalTickets;
                } else { 0 };
                
                // Create updated event
                let updatedEvent : Event = {
                    eventId = event.eventId;
                    name = Option.get(name, event.name);
                    date = Option.get(date, event.date);
                    venue = Option.get(venue, event.venue);
                    price = Option.get(price, event.price);
                    totalTickets = newTotalTickets;
                    availableTickets = event.availableTickets + additionalTickets;
                    organizer = event.organizer;
                    isActive = Option.get(isActive, event.isActive);
                    description = Option.get(description, event.description);
                    imageUrl = switch (imageUrl) {
                        case (?value) { ?value };
                        case null { event.imageUrl };
                    };
                };
                
                events.put(eventId, updatedEvent);
                #ok(updatedEvent);
            };
            case null { #err(#NotFound) };
        };
    };
    
    // Cancel event (only organizer or admin)
    public shared(msg) func cancelEvent(eventId: Nat) : async BoolResponse {
        switch(events.get(eventId)) {
            case (?event) {
                // Verify caller is the organizer or admin
                if (msg.caller != event.organizer and 
                    Option.get(userRoles.get(msg.caller), #User) != #Admin) {
                    return #err(#NotAuthorized);
                };
                
                // Update event to inactive
                let updatedEvent : Event = {
                    eventId = event.eventId;
                    name = event.name;
                    date = event.date;
                    venue = event.venue;
                    price = event.price;
                    totalTickets = event.totalTickets;
                    availableTickets = event.availableTickets;
                    organizer = event.organizer;
                    isActive = false;
                    description = event.description;
                    imageUrl = event.imageUrl;
                };
                
                events.put(eventId, updatedEvent);
                
                // Here you might also implement refund logic for purchased tickets
                
                #ok(true);
            };
            case null { #err(#NotFound) };
        };
    };
    
    // === Ticket Management Functions ===
    
    // Purchase a ticket
    public shared(msg) func purchaseTicket(eventId: Nat) : async TicketResponse {
        switch(events.get(eventId)) {
            case (?event) {
                // Check if event is active and tickets are available
                if (not event.isActive) {
                    return #err(#InvalidOperation);
                };
                
                if (event.availableTickets == 0) {
                    return #err(#SoldOut);
                };
                
                if (event.date < Time.now()) {
                    return #err(#InvalidOperation);
                };
                
                // In a real implementation, process payment here
                // For demo purposes, we'll skip actual ICP transfers
                
                // Create ticket transfer record
                let ticketTransfer = {
                    from = event.organizer;
                    to = msg.caller;
                    price = event.price;
                    timestamp = Time.now();
                };
                
                // Create ticket metadata
                let metadata : TicketMetadata = {
                    eventName = event.name;
                    ticketClass = "Standard";
                    seatInfo = null;
                    purchaseDate = Time.now();
                };
                
                // Mint new ticket as NFT
                let tokenId = nextTicketId;
                nextTicketId += 1;
                
                let newTicket : Ticket = {
                    tokenId = tokenId;
                    eventId = eventId;
                    owner = msg.caller;
                    originalPrice = event.price;
                    currentPrice = event.price;
                    purchaseHistory = [ticketTransfer];
                    isValid = true;
                    metadata = ?metadata;
                };
                
                tickets.put(tokenId, newTicket);
                
                // Update available tickets for the event
                let updatedEvent : Event = {
                    eventId = event.eventId;
                    name = event.name;
                    date = event.date;
                    venue = event.venue;
                    price = event.price;
                    totalTickets = event.totalTickets;
                    availableTickets = event.availableTickets - 1;
                    organizer = event.organizer;
                    isActive = event.isActive;
                    description = event.description;
                    imageUrl = event.imageUrl;
                };
                
                events.put(eventId, updatedEvent);
                
                // Add ticket to user's collection
                let userTicketsList = Option.get(userTickets.get(msg.caller), []);
                userTickets.put(msg.caller, Array.append(userTicketsList, [tokenId]));
                
                #ok(newTicket);
            };
            case null { #err(#NotFound) };
        };
    };

    // Get ticket details by ID
    public query func getTicket(tokenId: Nat) : async TicketResponse {
        switch(tickets.get(tokenId)) {
            case (?ticket) { #ok(ticket) };
            case null { #err(#NotFound) };
        };
    };
    
    // Verify ticket ownership and validity
    public query func verifyTicket(tokenId: Nat, owner: Principal) : async BoolResponse {
        switch(tickets.get(tokenId)) {
            case (?ticket) {
                let isValid = ticket.isValid and ticket.owner == owner;
                #ok(isValid);
            };
            case null { #err(#NotFound) };
        };
    };
    
    // Get all tickets for an event
    public query func getEventTickets(eventId: Nat) : async TicketsResponse {
        let ticketsBuffer = Buffer.Buffer<Ticket>(0);
        
        for (ticket in tickets.vals()) {
            if (ticket.eventId == eventId) {
                ticketsBuffer.add(ticket);
            };
        };
        
        #ok(Buffer.toArray(ticketsBuffer));
    };
    
    // Get all tickets owned by a user
    public query func getUserTickets(user: Principal) : async TicketsResponse {
        switch(userTickets.get(user)) {
            case (?tokenIds) {
                let ticketsBuffer = Buffer.Buffer<Ticket>(0);
                
                for (tokenId in tokenIds.vals()) {
                    switch(tickets.get(tokenId)) {
                        case (?ticket) { ticketsBuffer.add(ticket) };
                        case null {};
                    };
                };
                
                #ok(Buffer.toArray(ticketsBuffer));
            };
            case null { #ok([]) };
        };
    };
    
    // List ticket for resale
    public shared(msg) func listTicketForResale(tokenId: Nat, price: Nat) : async TicketResponse {
        switch(tickets.get(tokenId)) {
            case (?ticket) {
                // Verify ticket ownership
                if (msg.caller != ticket.owner) {
                    return #err(#NotAuthorized);
                };
                
                // Check if price is within allowed limits (1.2x original price)
                let maxAllowedPrice = Float.toInt(Float.fromInt(ticket.originalPrice) * MAX_RESALE_MULTIPLIER);
                if (price > maxAllowedPrice) {
    return #err(#LimitExceeded);
};
                
                // Update ticket price
                let updatedTicket : Ticket = {
                    tokenId = ticket.tokenId;
                    eventId = ticket.eventId;
                    owner = ticket.owner;
                    originalPrice = ticket.originalPrice;
                    currentPrice = price;
                    purchaseHistory = ticket.purchaseHistory;
                    isValid = ticket.isValid;
                    metadata = ticket.metadata;
                };
                
                tickets.put(tokenId, updatedTicket);
                #ok(updatedTicket);
            };
            case null { #err(#NotFound) };
        };
    };
    
    // Buy a resale ticket
    public shared(msg) func buyResaleTicket(tokenId: Nat) : async TicketResponse {
        switch(tickets.get(tokenId)) {
            case (?ticket) {
                // Check if buyer is trying to buy their own ticket
                if (msg.caller == ticket.owner) {
                    return #err(#InvalidOperation);
                };
                
                // Get event details to check if the event is still active
                switch(events.get(ticket.eventId)) {
                    case (?event) {
                        if (not event.isActive or event.date < Time.now()) {
                            return #err(#InvalidOperation);
                        };
                    };
                    case null { return #err(#NotFound) };
                };
                
                // In a real implementation, process payment here
                // For demo purposes, we'll skip actual ICP transfers
                
                // Create ticket transfer record
                let ticketTransfer = {
                    from = ticket.owner;
                    to = msg.caller;
                    price = ticket.currentPrice;
                    timestamp = Time.now();
                };
                
                // Update ticket ownership and history
                let updatedTicket : Ticket = {
                    tokenId = ticket.tokenId;
                    eventId = ticket.eventId;
                    owner = msg.caller;
                    originalPrice = ticket.originalPrice;
                    currentPrice = ticket.currentPrice;
                    purchaseHistory = Array.append(ticket.purchaseHistory, [ticketTransfer]);
                    isValid = ticket.isValid;
                    metadata = ticket.metadata;
                };
                
                tickets.put(tokenId, updatedTicket);
                
                // Remove ticket from previous owner's collection
                switch(userTickets.get(ticket.owner)) {
                    case (?tokenIds) {
                        let updatedTokenIds = Array.filter<Nat>(tokenIds, func(id) { id != tokenId });
                        userTickets.put(ticket.owner, updatedTokenIds);
                    };
                    case null {};
                };
                
                // Add ticket to new owner's collection
                let buyerTickets = Option.get(userTickets.get(msg.caller), []);
                userTickets.put(msg.caller, Array.append(buyerTickets, [tokenId]));
                
                #ok(updatedTicket);
            };
            case null { #err(#NotFound) };
        };
    };
    
    // Transfer ticket to another user (gift)
    public shared(msg) func transferTicket(tokenId: Nat, recipient: Principal) : async TicketResponse {
        switch(tickets.get(tokenId)) {
            case (?ticket) {
                // Verify ticket ownership
                if (msg.caller != ticket.owner) {
                    return #err(#NotAuthorized);
                };
                
                // Check if recipient is the current owner
                if (recipient == ticket.owner) {
                    return #err(#InvalidOperation);
                };
                
                // Create ticket transfer record (with zero price since it's a gift)
                let ticketTransfer = {
                    from = ticket.owner;
                    to = recipient;
                    price = 0;
                    timestamp = Time.now();
                };
                
                // Update ticket ownership and history
                let updatedTicket : Ticket = {
                    tokenId = ticket.tokenId;
                    eventId = ticket.eventId;
                    owner = recipient;
                    originalPrice = ticket.originalPrice;
                    currentPrice = ticket.currentPrice;
                    purchaseHistory = Array.append(ticket.purchaseHistory, [ticketTransfer]);
                    isValid = ticket.isValid;
                    metadata = ticket.metadata;
                };
                
                tickets.put(tokenId, updatedTicket);
                
                // Remove ticket from previous owner's collection
                switch(userTickets.get(ticket.owner)) {
                    case (?tokenIds) {
                        let updatedTokenIds = Array.filter<Nat>(tokenIds, func(id) { id != tokenId });
                        userTickets.put(ticket.owner, updatedTokenIds);
                    };
                    case null {};
                };
                
                // Add ticket to new owner's collection
                let recipientTickets = Option.get(userTickets.get(recipient), []);
                userTickets.put(recipient, Array.append(recipientTickets, [tokenId]));
                
                #ok(updatedTicket);
            };
            case null { #err(#NotFound) };
        };
    };
    
    // Invalidate a ticket (e.g., after use)
    public shared(msg) func invalidateTicket(tokenId: Nat) : async TicketResponse {
        switch(tickets.get(tokenId)) {
            case (?ticket) {
                // Get event to check if caller is the organizer
                switch(events.get(ticket.eventId)) {
                    case (?event) {
                        if (msg.caller != event.organizer and msg.caller != owner) {
                            return #err(#NotAuthorized);
                        };
                        
                        // Update ticket validity
                        let updatedTicket : Ticket = {
                            tokenId = ticket.tokenId;
                            eventId = ticket.eventId;
                            owner = ticket.owner;
                            originalPrice = ticket.originalPrice;
                            currentPrice = ticket.currentPrice;
                            purchaseHistory = ticket.purchaseHistory;
                            isValid = false;
                            metadata = ticket.metadata;
                        };
                        
                        tickets.put(tokenId, updatedTicket);
                        #ok(updatedTicket);
                    };
                    case null { #err(#NotFound) };
                };
            };
            case null { #err(#NotFound) };
        };
    };
    
    // === User Role Management ===
    
    // Assign a role to a user (admin only)
    public shared(msg) func assignRole(user: Principal, role: Role) : async BoolResponse {
        // Verify caller is admin
        if (Option.get(userRoles.get(msg.caller), #User) != #Admin) {
            return #err(#NotAuthorized);
        };
        
        userRoles.put(user, role);
        #ok(true);
    };
    
    // Get user role
    public query func getUserRole(user: Principal) : async Text {
        switch(userRoles.get(user)) {
            case (?#Admin) { "Admin" };
            case (?#Organizer) { "Organizer" };
            case (?#User) { "User" };
            case null { "User" }; // Default role
        };
    };
    
    // === DIP721 NFT Implementation ===
    
    // Get NFT metadata (DIP721 compliance)
    public query func getTokenMetadata(tokenId: Nat) : async ?TokenMetadata {
        switch(tickets.get(tokenId)) {
            case (?ticket) {
                let metadata = ?{
                    tokenId = tokenId;
                    owner = ticket.owner;
                    metadata = null; // Could encode ticket details as Blob
                    properties = [
                        ("eventId", Nat.toText(ticket.eventId)),
                        ("isValid", if (ticket.isValid) "true" else "false")
                    ];
                    isApproved = false;
                };
                
                return metadata;
            };
            case null { null };
        };
    };
    
    // Get total supply of NFTs
    public query func totalSupply() : async Nat {
        return nextTicketId - 1;
    };
    
    // Balance of (number of tickets owned by a principal)
    public query func balanceOf(owner: Principal) : async Nat {
        switch(userTickets.get(owner)) {
            case (?tokenIds) { tokenIds.size() };
            case null { 0 };
        };
    };
    
    // Owner of a specific token
    public query func ownerOf(tokenId: Nat) : async ?Principal {
        switch(tickets.get(tokenId)) {
            case (?ticket) { ?ticket.owner };
            case null { null };
        };
    };
    
    // Get all tokens owned by an address
    public query func tokensOf(owner: Principal) : async [Nat] {
        switch(userTickets.get(owner)) {
            case (?tokenIds) { tokenIds };
            case null { [] };
        };
    };
    
    // === Organizer Functions ===
    
    // Get all events for an organizer
    public query func getOrganizerEvents(organizer: Principal) : async EventsResponse {
        switch(organizerEvents.get(organizer)) {
            case (?eventIds) {
                let eventsBuffer = Buffer.Buffer<Event>(0);
                
                for (eventId in eventIds.vals()) {
                    switch(events.get(eventId)) {
                        case (?event) { eventsBuffer.add(event) };
                        case null {};
                    };
                };
                
                #ok(Buffer.toArray(eventsBuffer));
            };
            case null { #ok([]) };
        };
    };
    
    // Get event statistics (tickets sold, revenue, etc.)
    public shared(msg) func getEventStats(eventId: Nat) : async Result.Result<{
        totalSold: Nat;
        totalRevenue: Nat;
        validTickets: Nat;
    }, Error> {
        switch(events.get(eventId)) {
            case (?event) {
                // Verify caller is the organizer or admin
                if (msg.caller != event.organizer and 
                    Option.get(userRoles.get(msg.caller), #User) != #Admin) {
                    return #err(#NotAuthorized);
                };
                
                var totalSold = 0;
                var totalRevenue = 0;
                var validTickets = 0;
                
                for (ticket in tickets.vals()) {
                    if (ticket.eventId == eventId) {
                        totalSold += 1;
                        totalRevenue += ticket.originalPrice;
                        
                        if (ticket.isValid) {
                            validTickets += 1;
                        };
                    };
                };
                
                #ok({
                    totalSold = totalSold;
                    totalRevenue = totalRevenue;
                    validTickets = validTickets;
                });
            };
            case null { #err(#NotFound) };
        };
    };
    
    // === System Functions ===
    
    // Check if a system upgrade is needed
    system func preupgrade() {
        // Persistence logic would go here when using stable storage
    };
    
    system func postupgrade() {
        // Initialization after upgrade would go here
    };
}