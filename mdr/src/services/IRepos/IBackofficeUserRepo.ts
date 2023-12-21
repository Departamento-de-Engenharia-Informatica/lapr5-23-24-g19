import { Repo } from '../../core/infra/Repo'
import { Email } from '../../domain/user/email'
import BackofficeUser from '../../domain/user/backofficeUser/backofficeUser'

export default interface IBackofficeUserRepo extends Repo<BackofficeUser> {
    save(user: BackofficeUser): Promise<BackofficeUser>
    find(id: Email): Promise<BackofficeUser>
    delete(user: BackofficeUser): Promise<BackofficeUser>
    existsWithEmail(t: Email): Promise<boolean>
}
