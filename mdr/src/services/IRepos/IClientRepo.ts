import { Repo } from '../../core/infra/Repo'
import Client from '../../domain/user/client/Client'
import { Email } from '../../domain/user/email'

export default interface IClientRepo extends Repo<Client> {
    save(user: Client): Promise<Client>
    find(id: Email): Promise<Client>
    findByState(state: string): Promise<Client[]>
    delete(user: Client): Promise<boolean>
    existsWithEmail(t: Email): Promise<boolean>
}
