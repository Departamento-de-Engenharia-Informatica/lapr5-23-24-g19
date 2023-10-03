export interface IUserPersistence {
	_id: string;
	firstName: string;
	lastName: string;
	email: string;
	password: string;
	salt: string;
	role: string
  }