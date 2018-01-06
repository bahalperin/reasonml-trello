type movement =
  | Started
  | Moving;

type listDropLocation = int;

type cardDropLocation = (CardList.cid, int);

type dropLocation =
  | List(listDropLocation)
  | Card(cardDropLocation);

type item('item, 'location) = {
  item: 'item,
  dropLocation: 'location
};

type list_ = item(CardList.t, listDropLocation);

type card = item(Card.t, cardDropLocation);

type target =
  | List(list_)
  | Card(card);

type t_ = {
  movement,
  target,
  initialClickOffset: (int, int),
  mousePosition: (int, int)
};

type t = option(t_);

let init = () => None;