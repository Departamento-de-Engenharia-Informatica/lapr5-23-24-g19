import { Entity } from '../core/domain/Entity'
import { UniqueEntityID } from '../core/domain/UniqueEntityID'

export class BuildingId extends Entity<any> {
    get id(): UniqueEntityID {
        return this._id
    }

    private constructor(id?: UniqueEntityID) {
        super(null, id)
    }
}
