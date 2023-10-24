import { Repo } from '../../core/infra/Repo'
import { Passage } from '../../domain/passage/passage'

export default interface IPassageRepo extends Repo<Passage> {
    save(passage: Passage): Promise<Passage>
    exists(passage: Passage | string): Promise<boolean>
}
