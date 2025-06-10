import React, { useState, useEffect } from 'react';
// import { Actor, HttpAgent } from '@dfinity/agent';
import { Principal } from '@dfinity/principal';
import { HBT_ticketing_system_backend } from '../../declarations/HBT_ticketing_system_backend';

const App = () => {
  // State variables
  const [isAuthenticated, setIsAuthenticated] = useState(false);
  const [principal, setPrincipal] = useState(null);
  const [userRole, setUserRole] = useState('User');
  const [events, setEvents] = useState([]);
  const [userTickets, setUserTickets] = useState([]);
  const [organizerEvents, setOrganizerEvents] = useState([]);
  const [selectedEvent, setSelectedEvent] = useState(null);
  const [selectedTicket, setSelectedTicket] = useState(null);
  const [activeTab, setActiveTab] = useState('events');
  const [isLoading, setIsLoading] = useState(false);
  const [notification, setNotification] = useState({ show: false, message: '', type: '' });

  // Form states
  const [newEventForm, setNewEventForm] = useState({
    name: '',
    date: '',
    venue: '',
    price: '',
    totalTickets: '',
    description: '',
    imageUrl: ''
  });
  const [resaleForm, setResaleForm] = useState({
    price: ''
  });
  const [transferForm, setTransferForm] = useState({
    recipient: ''
  });

  // Initialize the agent
  useEffect(() => {
    const initAgent = async () => {
      try {
        // For local development
        const agent = new HttpAgent({ host: 'http://localhost:8000' });
        
        // Fetch root key for local development
        if (process.env.NODE_ENV !== 'production') {
          await agent.fetchRootKey();
        }
      } catch (error) {
        console.error('Failed to initialize agent:', error);
        showNotification('Failed to connect to the Internet Computer', 'error');
      }
    };

    initAgent();
  }, []);

  // Authentication
  const login = async () => {
    try {
      // In a real application, this would use Internet Identity or another authentication method
      const authClient = await window.ic?.auth?.getAuthClient();
      
      if (authClient) {
        if (!authClient.isAuthenticated()) {
          await authClient.login({
            onSuccess: async () => {
              const identity = authClient.getIdentity();
              const userPrincipal = identity.getPrincipal();
              setPrincipal(userPrincipal);
              setIsAuthenticated(true);
              
              // Get user role using the imported backend
              try {
                const role = await HBT_ticketing_system_backend.getUserRole(userPrincipal);
                setUserRole(role);
              } catch (error) {
                console.error('Failed to get user role:', error);
              }
              
              showNotification('Successfully logged in', 'success');
            }
          });
        } else {
          const identity = authClient.getIdentity();
          const userPrincipal = identity.getPrincipal();
          setPrincipal(userPrincipal);
          setIsAuthenticated(true);
          
          try {
            const role = await HBT_ticketing_system_backend.getUserRole(userPrincipal);
            setUserRole(role);
          } catch (error) {
            console.error('Failed to get user role:', error);
          }
          
          showNotification('Already logged in', 'success');
        }
      } else {
        // Simulate login for demo purposes
        const mockPrincipal = Principal.fromText('2vxsx-fae');
        setPrincipal(mockPrincipal);
        setIsAuthenticated(true);
        showNotification('Simulated login successful', 'success');
      }
    } catch (error) {
      console.error('Login failed:', error);
      showNotification('Login failed', 'error');
    }
  };

  const logout = async () => {
    try {
      const authClient = await window.ic?.auth?.getAuthClient();
      if (authClient) {
        await authClient.logout();
      }
      setPrincipal(null);
      setIsAuthenticated(false);
      setUserRole('User');
      showNotification('Logged out successfully', 'success');
    } catch (error) {
      console.error('Logout failed:', error);
      showNotification('Logout failed', 'error');
    }
  };

  // Load data
  useEffect(() => {
    if (isAuthenticated) {
      loadEvents();
      loadUserTickets();
      if (userRole === 'Organizer' || userRole === 'Admin') {
        loadOrganizerEvents();
      }
    }
  }, [isAuthenticated, userRole]);

  const loadEvents = async () => {
    setIsLoading(true);
    try {
      const result = await HBT_ticketing_system_backend.getActiveEvents();
      if ('ok' in result) {
        setEvents(result.ok);
      } else {
        console.error('Error fetching events:', result.err);
        showNotification('Failed to load events', 'error');
      }
    } catch (error) {
      console.error('Error fetching events:', error);
      showNotification('Failed to load events', 'error');
    } finally {
      setIsLoading(false);
    }
  };

  const loadUserTickets = async () => {
    if (!principal) return;
    
    setIsLoading(true);
    try {
      const result = await HBT_ticketing_system_backend.getUserTickets(principal);
      if ('ok' in result) {
        setUserTickets(result.ok);
      } else {
        console.error('Error fetching user tickets:', result.err);
        showNotification('Failed to load your tickets', 'error');
      }
    } catch (error) {
      console.error('Error fetching user tickets:', error);
      showNotification('Failed to load your tickets', 'error');
    } finally {
      setIsLoading(false);
    }
  };

  const loadOrganizerEvents = async () => {
    if (!principal) return;
    
    setIsLoading(true);
    try {
      const result = await HBT_ticketing_system_backend.getOrganizerEvents(principal);
      if ('ok' in result) {
        setOrganizerEvents(result.ok);
      } else {
        console.error('Error fetching organizer events:', result.err);
        showNotification('Failed to load your events', 'error');
      }
    } catch (error) {
      console.error('Error fetching organizer events:', error);
      showNotification('Failed to load your events', 'error');
    } finally {
      setIsLoading(false);
    }
  };

  // Event handlers
  const handleInputChange = (e, formSetter, formState) => {
    const { name, value } = e.target;
    formSetter({
      ...formState,
      [name]: value
    });
  };

  const handleCreateEvent = async (e) => {
    e.preventDefault();
    
    setIsLoading(true);
    try {
      // Convert string values to appropriate types
      const eventData = {
        name: newEventForm.name,
        date: BigInt(new Date(newEventForm.date).getTime() * 1000000), // Convert to nanoseconds
        venue: newEventForm.venue,
        price: BigInt(Number(newEventForm.price) * 100000000), // Convert to e8s
        totalTickets: BigInt(newEventForm.totalTickets),
        description: newEventForm.description,
        imageUrl: newEventForm.imageUrl ? [newEventForm.imageUrl] : [] // Convert to opt
      };
      
      const result = await HBT_ticketing_system_backend.createEvent(
        eventData.name,
        eventData.date,
        eventData.venue,
        eventData.price,
        eventData.totalTickets,
        eventData.description,
        eventData.imageUrl.length > 0 ? eventData.imageUrl[0] : null
      );
      
      if ('ok' in result) {
        showNotification('Event created successfully', 'success');
        loadEvents();
        loadOrganizerEvents();
        
        // Reset form
        setNewEventForm({
          name: '',
          date: '',
          venue: '',
          price: '',
          totalTickets: '',
          description: '',
          imageUrl: ''
        });
        
        // Switch to events tab
        setActiveTab('myEvents');
      } else {
        console.error('Error creating event:', result.err);
        showNotification('Failed to create event: ' + JSON.stringify(result.err), 'error');
      }
    } catch (error) {
      console.error('Error creating event:', error);
      showNotification('Failed to create event', 'error');
    } finally {
      setIsLoading(false);
    }
  };

  const handlePurchaseTicket = async (eventId) => {
    setIsLoading(true);
    try {
      const result = await HBT_ticketing_system_backend.purchaseTicket(eventId);
      if ('ok' in result) {
        showNotification('Ticket purchased successfully', 'success');
        loadEvents();
        loadUserTickets();
      } else {
        console.error('Error purchasing ticket:', result.err);
        showNotification('Failed to purchase ticket: ' + JSON.stringify(result.err), 'error');
      }
    } catch (error) {
      console.error('Error purchasing ticket:', error);
      showNotification('Failed to purchase ticket', 'error');
    } finally {
      setIsLoading(false);
    }
  };

  const handleListTicketForResale = async (e) => {
    e.preventDefault();
    if (!selectedTicket) return;
    
    setIsLoading(true);
    try {
      const result = await HBT_ticketing_system_backend.listTicketForResale(
        selectedTicket.tokenId,
        BigInt(Number(resaleForm.price) * 100000000) // Convert to e8s
      );
      
      if ('ok' in result) {
        showNotification('Ticket listed for resale', 'success');
        loadUserTickets();
        setResaleForm({ price: '' });
        setSelectedTicket(null);
      } else {
        console.error('Error listing ticket:', result.err);
        showNotification('Failed to list ticket: ' + JSON.stringify(result.err), 'error');
      }
    } catch (error) {
      console.error('Error listing ticket:', error);
      showNotification('Failed to list ticket', 'error');
    } finally {
      setIsLoading(false);
    }
  };

  const handleBuyResaleTicket = async (tokenId) => {
    setIsLoading(true);
    try {
      const result = await HBT_ticketing_system_backend.buyResaleTicket(tokenId);
      if ('ok' in result) {
        showNotification('Resale ticket purchased successfully', 'success');
        loadEvents();
        loadUserTickets();
      } else {
        console.error('Error buying resale ticket:', result.err);
        showNotification('Failed to buy resale ticket: ' + JSON.stringify(result.err), 'error');
      }
    } catch (error) {
      console.error('Error buying resale ticket:', error);
      showNotification('Failed to buy resale ticket', 'error');
    } finally {
      setIsLoading(false);
    }
  };

  const handleTransferTicket = async (e) => {
    e.preventDefault();
    if (!selectedTicket) return;
    
    setIsLoading(true);
    try {
      const recipientPrincipal = Principal.fromText(transferForm.recipient);
      const result = await HBT_ticketing_system_backend.transferTicket(selectedTicket.tokenId, recipientPrincipal);
      
      if ('ok' in result) {
        showNotification('Ticket transferred successfully', 'success');
        loadUserTickets();
        setTransferForm({ recipient: '' });
        setSelectedTicket(null);
      } else {
        console.error('Error transferring ticket:', result.err);
        showNotification('Failed to transfer ticket: ' + JSON.stringify(result.err), 'error');
      }
    } catch (error) {
      console.error('Error transferring ticket:', error);
      showNotification('Failed to transfer ticket', 'error');
    } finally {
      setIsLoading(false);
    }
  };

  const handleCancelEvent = async (eventId) => {
    if (!confirm('Are you sure you want to cancel this event?')) {
      return;
    }
    
    setIsLoading(true);
    try {
      const result = await HBT_ticketing_system_backend.cancelEvent(eventId);
      if ('ok' in result) {
        showNotification('Event cancelled successfully', 'success');
        loadEvents();
        loadOrganizerEvents();
      } else {
        console.error('Error cancelling event:', result.err);
        showNotification('Failed to cancel event: ' + JSON.stringify(result.err), 'error');
      }
    } catch (error) {
      console.error('Error cancelling event:', error);
      showNotification('Failed to cancel event', 'error');
    } finally {
      setIsLoading(false);
    }
  };

  const handleInvalidateTicket = async (tokenId) => {
    setIsLoading(true);
    try {
      const result = await HBT_ticketing_system_backend.invalidateTicket(tokenId);
      if ('ok' in result) {
        showNotification('Ticket invalidated successfully', 'success');
        loadOrganizerEvents();
      } else {
        console.error('Error invalidating ticket:', result.err);
        showNotification('Failed to invalidate ticket: ' + JSON.stringify(result.err), 'error');
      }
    } catch (error) {
      console.error('Error invalidating ticket:', error);
      showNotification('Failed to invalidate ticket', 'error');
    } finally {
      setIsLoading(false);
    }
  };

  // Utility functions
  const showNotification = (message, type = 'info') => {
    setNotification({ show: true, message, type });
    setTimeout(() => setNotification({ show: false, message: '', type: '' }), 5000);
  };

  const formatDate = (timestamp) => {
    // Convert nanoseconds to milliseconds
    const date = new Date(Number(timestamp) / 1000000);
    return date.toLocaleString();
  };

  const formatPrice = (price) => {
    // Convert e8s to ICP
    return (Number(price) / 100000000).toFixed(8) + ' ICP';
  };

  // Render functions
  const renderEvents = () => {
    if (events.length === 0) {
      return (
        <div className="text-center p-4">
          <p>No active events found.</p>
        </div>
      );
    }

    return (
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
        {events.map(event => (
          <div key={event.eventId.toString()} className="border rounded-lg shadow p-4">
            {event.imageUrl.length > 0 && (
              <img 
                src={event.imageUrl[0]} 
                alt={event.name} 
                className="w-full h-40 object-cover rounded-t-lg mb-2"
              />
            )}
            <h3 className="text-xl font-bold">{event.name}</h3>
            <p><strong>Date:</strong> {formatDate(event.date)}</p>
            <p><strong>Venue:</strong> {event.venue}</p>
            <p><strong>Price:</strong> {formatPrice(event.price)}</p>
            <p><strong>Available:</strong> {event.availableTickets.toString()}/{event.totalTickets.toString()}</p>
            <p className="mt-2">{event.description}</p>
            {isAuthenticated && (
              <button
                className="mt-4 bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded w-full"
                onClick={() => handlePurchaseTicket(event.eventId)}
                disabled={isLoading || Number(event.availableTickets) === 0}
              >
                {isLoading ? 'Processing...' : 'Purchase Ticket'}
              </button>
            )}
          </div>
        ))}
      </div>
    );
  };

  const renderUserTickets = () => {
    if (userTickets.length === 0) {
      return (
        <div className="text-center p-4">
          <p>You don't have any tickets yet.</p>
        </div>
      );
    }

    return (
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
        {userTickets.map(ticket => (
          <div key={ticket.tokenId.toString()} className="border rounded-lg shadow p-4">
            <h3 className="text-xl font-bold">
              {ticket.metadata[0]?.eventName || `Ticket #${ticket.tokenId.toString()}`}
            </h3>
            <p><strong>Token ID:</strong> {ticket.tokenId.toString()}</p>
            <p><strong>Event ID:</strong> {ticket.eventId.toString()}</p>
            <p><strong>Original Price:</strong> {formatPrice(ticket.originalPrice)}</p>
            <p><strong>Current Price:</strong> {formatPrice(ticket.currentPrice)}</p>
            <p><strong>Status:</strong> {ticket.isValid ? 'Valid' : 'Invalid'}</p>
            <div className="mt-4 flex space-x-2">
              <button
                className="bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded flex-1"
                onClick={() => {
                  setSelectedTicket(ticket);
                  setActiveTab('resaleTicket');
                }}
                disabled={!ticket.isValid}
              >
                Resell
              </button>
              <button
                className="bg-purple-500 hover:bg-purple-700 text-white font-bold py-2 px-4 rounded flex-1"
                onClick={() => {
                  setSelectedTicket(ticket);
                  setActiveTab('transferTicket');
                }}
                disabled={!ticket.isValid}
              >
                Transfer
              </button>
            </div>
          </div>
        ))}
      </div>
    );
  };

  const renderOrganizerEvents = () => {
    if (organizerEvents.length === 0) {
      return (
        <div className="text-center p-4">
          <p>You haven't created any events yet.</p>
        </div>
      );
    }

    return (
      <div className="space-y-4">
        {organizerEvents.map(event => (
          <div key={event.eventId.toString()} className="border rounded-lg shadow p-4">
            <div className="flex justify-between items-start">
              <div>
                <h3 className="text-xl font-bold">{event.name}</h3>
                <p><strong>Date:</strong> {formatDate(event.date)}</p>
                <p><strong>Venue:</strong> {event.venue}</p>
                <p><strong>Price:</strong> {formatPrice(event.price)}</p>
                <p><strong>Tickets:</strong> {event.availableTickets.toString()}/{event.totalTickets.toString()} available</p>
                <p><strong>Status:</strong> {event.isActive ? 'Active' : 'Cancelled'}</p>
              </div>
              <div className="flex flex-col space-y-2">
                <button
                  className="bg-red-500 hover:bg-red-700 text-white font-bold py-1 px-3 rounded"
                  onClick={() => handleCancelEvent(event.eventId)}
                  disabled={!event.isActive || isLoading}
                >
                  Cancel Event
                </button>
                <button
                  className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-1 px-3 rounded"
                  onClick={() => {
                    setSelectedEvent(event);
                    setActiveTab('eventDetails');
                  }}
                >
                  View Details
                </button>
              </div>
            </div>
          </div>
        ))}
      </div>
    );
  };

  const renderEventDetails = () => {
    if (!selectedEvent) {
      return (
        <div className="text-center p-4">
          <p>No event selected.</p>
          <button
            className="mt-4 bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded"
            onClick={() => setActiveTab('myEvents')}
          >
            Back to My Events
          </button>
        </div>
      );
    }

    return (
      <div className="space-y-4">
        <h2 className="text-2xl font-bold">{selectedEvent.name} - Event Details</h2>
        
        <div className="grid grid-cols-1 md:grid-cols-2 gap-4">
          <div className="border rounded-lg shadow p-4">
            <h3 className="text-lg font-bold mb-2">Event Information</h3>
            <p><strong>Event ID:</strong> {selectedEvent.eventId.toString()}</p>
            <p><strong>Date:</strong> {formatDate(selectedEvent.date)}</p>
            <p><strong>Venue:</strong> {selectedEvent.venue}</p>
            <p><strong>Price:</strong> {formatPrice(selectedEvent.price)}</p>
            <p><strong>Total Tickets:</strong> {selectedEvent.totalTickets.toString()}</p>
            <p><strong>Available Tickets:</strong> {selectedEvent.availableTickets.toString()}</p>
            <p><strong>Status:</strong> {selectedEvent.isActive ? 'Active' : 'Cancelled'}</p>
            <p><strong>Description:</strong> {selectedEvent.description}</p>
          </div>
          
          <div className="border rounded-lg shadow p-4">
            <h3 className="text-lg font-bold mb-2">Event Statistics</h3>
            <p><strong>Tickets Sold:</strong> {(Number(selectedEvent.totalTickets) - Number(selectedEvent.availableTickets)).toString()}</p>
            <p><strong>Revenue:</strong> {formatPrice(BigInt(Number(selectedEvent.price) * (Number(selectedEvent.totalTickets) - Number(selectedEvent.availableTickets))))}</p>
            
            <div className="mt-4">
              <button
                className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded w-full mb-2"
                onClick={() => {/* Implement view attendees */}}
              >
                View Attendees
              </button>
              <button
                className="bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded w-full mb-2"
                onClick={() => {/* Implement scan tickets */}}
              >
                Scan Tickets
              </button>
              <button
                className="bg-purple-500 hover:bg-purple-700 text-white font-bold py-2 px-4 rounded w-full"
                onClick={() => {/* Implement event update */}}
              >
                Update Event
              </button>
            </div>
          </div>
        </div>
        
        <button
          className="mt-4 bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded"
          onClick={() => {
            setSelectedEvent(null);
            setActiveTab('myEvents');
          }}
        >
          Back to My Events
        </button>
      </div>
    );
  };

  const renderCreateEventForm = () => {
    return (
      <div className="max-w-md mx-auto">
        <h2 className="text-2xl font-bold mb-4">Create New Event</h2>
        <form onSubmit={handleCreateEvent} className="space-y-4">
          <div>
            <label className="block text-sm font-bold mb-1">Event Name</label>
            <input
              type="text"
              name="name"
              value={newEventForm.name}
              onChange={(e) => handleInputChange(e, setNewEventForm, newEventForm)}
              className="w-full px-3 py-2 border rounded"
              required
            />
          </div>
          
          <div>
            <label className="block text-sm font-bold mb-1">Date</label>
            <input
              type="datetime-local"
              name="date"
              value={newEventForm.date}
              onChange={(e) => handleInputChange(e, setNewEventForm, newEventForm)}
              className="w-full px-3 py-2 border rounded"
              required
            />
          </div>
          
          <div>
            <label className="block text-sm font-bold mb-1">Venue</label>
            <input
              type="text"
              name="venue"
              value={newEventForm.venue}
              onChange={(e) => handleInputChange(e, setNewEventForm, newEventForm)}
              className="w-full px-3 py-2 border rounded"
              required
            />
          </div>
          
          <div>
            <label className="block text-sm font-bold mb-1">Price (ICP)</label>
            <input
              type="number"
              name="price"
              value={newEventForm.price}
              onChange={(e) => handleInputChange(e, setNewEventForm, newEventForm)}
              className="w-full px-3 py-2 border rounded"
              min="0"
              step="0.00000001"
              required
            />
          </div>
          
          <div>
            <label className="block text-sm font-bold mb-1">Total Tickets</label>
            <input
              type="number"
              name="totalTickets"
              value={newEventForm.totalTickets}
              onChange={(e) => handleInputChange(e, setNewEventForm, newEventForm)}
              className="w-full px-3 py-2 border rounded"
              min="1"
              required
            />
          </div>
          
          <div>
            <label className="block text-sm font-bold mb-1">Description</label>
            <textarea
              name="description"
              value={newEventForm.description}
              onChange={(e) => handleInputChange(e, setNewEventForm, newEventForm)}
              className="w-full px-3 py-2 border rounded"
              rows="3"
              required
            />
          </div>
          
          <div>
            <label className="block text-sm font-bold mb-1">Image URL (optional)</label>
            <input
              type="url"
              name="imageUrl"
              value={newEventForm.imageUrl}
              onChange={(e) => handleInputChange(e, setNewEventForm, newEventForm)}
              className="w-full px-3 py-2 border rounded"
            />
          </div>
          
          <button
            type="submit"
            className="w-full bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
            disabled={isLoading}
          >
            {isLoading ? 'Creating...' : 'Create Event'}
          </button>
        </form>
      </div>
    );
  };

  const renderResaleTicketForm = () => {
    if (!selectedTicket) {
      return (
        <div className="text-center p-4">
          <p>No ticket selected.</p>
          <button
            className="mt-4 bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded"
            onClick={() => setActiveTab('myTickets')}
          >
            Back to My Tickets
          </button>
        </div>
      );
    }

    return (
      <div className="max-w-md mx-auto">
        <h2 className="text-2xl font-bold mb-4">List Ticket for Resale</h2>
        <div className="mb-4 p-4 border rounded">
          <p><strong>Ticket ID:</strong> {selectedTicket.tokenId.toString()}</p>
          <p><strong>Event:</strong> {selectedTicket.metadata[0]?.eventName || `Event #${selectedTicket.eventId.toString()}`}</p>
          <p><strong>Original Price:</strong> {formatPrice(selectedTicket.originalPrice)}</p>
          <p><strong>Current Price:</strong> {formatPrice(selectedTicket.currentPrice)}</p>
        </div>
        
        <form onSubmit={handleListTicketForResale} className="space-y-4">
          <div>
            <label className="block text-sm font-bold mb-1">Resale Price (ICP)</label>
            <input
              type="number"
              name="price"
              value={resaleForm.price}
              onChange={(e) => handleInputChange(e, setResaleForm, resaleForm)}
              className="w-full px-3 py-2 border rounded"
              min="0"
              step="0.00000001"
              required
            />
            <p className="text-sm text-gray-500 mt-1">
              Maximum allowed price: {formatPrice(BigInt(Number(selectedTicket.originalPrice) * 1.2))}
            </p>
          </div>
          
          <button
            type="submit"
            className="w-full bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
            disabled={isLoading}
          >
            {isLoading ? 'Processing...' : 'List for Resale'}
          </button>
          
          <button
            type="button"
            className="w-full bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded"
            onClick={() => {
              setSelectedTicket(null);
              setActiveTab('myTickets');
            }}
          >
            Cancel
          </button>
        </form>
      </div>
    );
  };

  const renderTransferTicketForm = () => {
    if (!selectedTicket) {
      return (
        <div className="text-center p-4">
          <p>No ticket selected.</p>
          <button
            className="mt-4 bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded"
            onClick={() => setActiveTab('myTickets')}
          >
            Back to My Tickets
          </button>
        </div>
      );
    }

    return (
      <div className="max-w-md mx-auto">
        <h2 className="text-2xl font-bold mb-4">Transfer Ticket</h2>
        <div className="mb-4 p-4 border rounded">
          <p><strong>Ticket ID:</strong> {selectedTicket.tokenId.toString()}</p>
          <p><strong>Event:</strong> {selectedTicket.metadata[0]?.eventName || `Event #${selectedTicket.eventId.toString()}`}</p>
          <p><strong>Original Price:</strong> {formatPrice(selectedTicket.originalPrice)}</p>
        </div>
        
        <form onSubmit={handleTransferTicket} className="space-y-4">
          <div>
            <label className="block text-sm font-bold mb-1">Recipient Principal ID</label>
            <input
              type="text"
              name="recipient"
              value={transferForm.recipient}
              onChange={(e) => handleInputChange(e, setTransferForm, transferForm)}
              className="w-full px-3 py-2 border rounded"
              placeholder="aaaaa-bbbbb-ccccc-ddddd-eee"
              required
            />
            <p className="text-sm text-gray-500 mt-1">
              Enter the principal ID of the person you want to transfer the ticket to.
            </p>
          </div>
          
          <button
            type="submit"
            className="w-full bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
            disabled={isLoading}
          >
            {isLoading ? 'Processing...' : 'Transfer Ticket'}
          </button>
          
          <button
            type="button"
            className="w-full bg-gray-500 hover:bg-gray-700 text-white font-bold py-2 px-4 rounded"
            onClick={() => {
              setSelectedTicket(null);
              setActiveTab('myTickets');
            }}
          >
            Cancel
          </button>
        </form>
      </div>
    );
  };

  const renderResaleMarketplace = () => {
    // Find all tickets that are listed for resale
    const resaleTickets = events.flatMap(event => 
      event.resaleTickets.map(ticket => ({
        ...ticket,
        eventName: event.name,
        eventDate: event.date,
        eventVenue: event.venue
      }))
    );

    if (resaleTickets.length === 0) {
      return (
        <div className="text-center p-4">
          <p>No tickets are currently available for resale.</p>
        </div>
      );
    }

    return (
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
        {resaleTickets.map(ticket => (
          <div key={ticket.tokenId.toString()} className="border rounded-lg shadow p-4">
            <h3 className="text-xl font-bold">{ticket.eventName}</h3>
            <p><strong>Date:</strong> {formatDate(ticket.eventDate)}</p>
            <p><strong>Venue:</strong> {ticket.eventVenue}</p>
            <p><strong>Original Price:</strong> {formatPrice(ticket.originalPrice)}</p>
            <p><strong>Resale Price:</strong> {formatPrice(ticket.resalePrice)}</p>
            {isAuthenticated && (
              <button
                className="mt-4 bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded w-full"
                onClick={() => handleBuyResaleTicket(ticket.tokenId)}
                disabled={isLoading}
              >
                {isLoading ? 'Processing...' : 'Buy Ticket'}
              </button>
            )}
          </div>
        ))}
      </div>
    );
  };

  const renderAdminPanel = () => {
    if (userRole !== 'Admin') {
      return (
        <div className="text-center p-4">
          <p>Access denied. Admin privileges required.</p>
        </div>
      );
    }

    return (
      <div className="space-y-6">
        <h2 className="text-2xl font-bold">Admin Panel</h2>
        
        <div className="border rounded-lg shadow p-4">
          <h3 className="text-xl font-bold mb-4">User Management</h3>
          <div className="space-y-4">
            <div>
              <label className="block text-sm font-bold mb-1">Assign Organizer Role</label>
              <div className="flex space-x-2">
                <input
                  type="text"
                  placeholder="Principal ID"
                  className="flex-1 px-3 py-2 border rounded"
                />
                <button
                  className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
                >
                  Assign
                </button>
              </div>
            </div>
            
            <div>
              <label className="block text-sm font-bold mb-1">Revoke Organizer Role</label>
              <div className="flex space-x-2">
                <input
                  type="text"
                  placeholder="Principal ID"
                  className="flex-1 px-3 py-2 border rounded"
                />
                <button
                  className="bg-red-500 hover:bg-red-700 text-white font-bold py-2 px-4 rounded"
                >
                  Revoke
                </button>
              </div>
            </div>
          </div>
        </div>
        
        <div className="border rounded-lg shadow p-4">
          <h3 className="text-xl font-bold mb-4">System Management</h3>
          <div className="space-y-4">
            <div className="flex space-x-2">
              <button
                className="flex-1 bg-purple-500 hover:bg-purple-700 text-white font-bold py-2 px-4 rounded"
              >
                Update Fee Settings
              </button>
              <button
                className="flex-1 bg-yellow-500 hover:bg-yellow-700 text-white font-bold py-2 px-4 rounded"
              >
                View System Metrics
              </button>
            </div>
            
            <div>
              <button
                className="w-full bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded"
              >
                Withdraw Platform Fees
              </button>
            </div>
          </div>
        </div>
      </div>
    );
  };

  // Main render
  return (
    <div className="min-h-screen bg-gray-100">
      {/* Header */}
      <header className="bg-blue-600 text-white p-4">
        <div className="container mx-auto flex justify-between items-center">
          <h1 className="text-2xl font-bold">IC Ticketing System</h1>
          <div>
            {isAuthenticated ? (
              <div className="flex items-center space-x-4">
                <span className="hidden md:inline">
                  {principal?.toString().substring(0, 8)}... ({userRole})
                </span>
                <button
                  onClick={logout}
                  className="bg-red-500 hover:bg-red-700 text-white font-bold py-1 px-3 rounded"
                >
                  Logout
                </button>
              </div>
            ) : (
              <button
                onClick={login}
                className="bg-green-500 hover:bg-green-700 text-white font-bold py-1 px-3 rounded"
              >
                Login
              </button>
            )}
          </div>
        </div>
      </header>

      {/* Notification */}
      {notification.show && (
        <div 
          className={`fixed top-4 right-4 p-4 rounded shadow-lg ${
            notification.type === 'success' ? 'bg-green-500' :
            notification.type === 'error' ? 'bg-red-500' : 'bg-blue-500'
          } text-white`}
        >
          {notification.message}
        </div>
      )}

      {/* Main content */}
      <main className="container mx-auto p-4">
        {/* Navigation Tabs */}
        <div className="flex flex-wrap mb-4">
          <button
            className={`px-4 py-2 ${activeTab === 'events' ? 'bg-blue-500 text-white' : 'bg-gray-200'}`}
            onClick={() => setActiveTab('events')}
          >
            Events
          </button>
          <button
            className={`px-4 py-2 ${activeTab === 'resaleMarketplace' ? 'bg-blue-500 text-white' : 'bg-gray-200'}`}
            onClick={() => setActiveTab('resaleMarketplace')}
          >
            Resale Marketplace
          </button>
          {isAuthenticated && (
            <>
              <button
                className={`px-4 py-2 ${activeTab === 'myTickets' ? 'bg-blue-500 text-white' : 'bg-gray-200'}`}
                onClick={() => setActiveTab('myTickets')}
              >
                My Tickets
              </button>
              {(userRole === 'Organizer' || userRole === 'Admin') && (
                <>
                  <button
                    className={`px-4 py-2 ${activeTab === 'myEvents' ? 'bg-blue-500 text-white' : 'bg-gray-200'}`}
                    onClick={() => setActiveTab('myEvents')}
                  >
                    My Events
                  </button>
                  <button
                    className={`px-4 py-2 ${activeTab === 'createEvent' ? 'bg-blue-500 text-white' : 'bg-gray-200'}`}
                    onClick={() => setActiveTab('createEvent')}
                  >
                    Create Event
                  </button>
                </>
              )}
              {userRole === 'Admin' && (
                <button
                  className={`px-4 py-2 ${activeTab === 'adminPanel' ? 'bg-blue-500 text-white' : 'bg-gray-200'}`}
                  onClick={() => setActiveTab('adminPanel')}
                >
                  Admin Panel
                </button>
              )}
            </>
          )}
        </div>

        {/* Content based on active tab */}
        <div className="bg-white p-4 rounded-lg shadow">
          {!isAuthenticated && (activeTab !== 'events' && activeTab !== 'resaleMarketplace') && (
            <div className="text-center p-4">
              <p>Please login to access this feature.</p>
            </div>
          )}

          {activeTab === 'events' && renderEvents()}
          {activeTab === 'resaleMarketplace' && renderResaleMarketplace()}
          
          {isAuthenticated && (
            <>
              {activeTab === 'myTickets' && renderUserTickets()}
              {activeTab === 'resaleTicket' && renderResaleTicketForm()}
              {activeTab === 'transferTicket' && renderTransferTicketForm()}
              
              {(userRole === 'Organizer' || userRole === 'Admin') && (
                <>
                  {activeTab === 'myEvents' && renderOrganizerEvents()}
                  {activeTab === 'createEvent' && renderCreateEventForm()}
                  {activeTab === 'eventDetails' && renderEventDetails()}
                </>
              )}
              
              {userRole === 'Admin' && activeTab === 'adminPanel' && renderAdminPanel()}
            </>
          )}
        </div>
      </main>

      {/* Footer */}
      <footer className="bg-gray-800 text-white p-4 mt-8">
        <div className="container mx-auto text-center">
          <p>&copy; {new Date().getFullYear()} IC Ticketing System. Powered by Internet Computer.</p>
        </div>
      </footer>
    </div>
  );
};

export default App;