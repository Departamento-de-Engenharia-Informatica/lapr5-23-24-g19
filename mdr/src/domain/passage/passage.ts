import { AggregateRoot } from '../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

import { Floor } from '../floor/floor'

interface PassageProps {
    floor1: Floor
    floor2: Floor
}

function invalidFloors(f1: Floor, f2: Floor) {
    return f1.equals(f2) || f1.building.equals(f2.building)
}

export class Passage extends AggregateRoot<PassageProps> {
    get id(): UniqueEntityID {
        return this._id
    }

    private constructor(props: PassageProps, id?: UniqueEntityID) {
        super(props, id)
    }

    public static create(props: PassageProps, id?: UniqueEntityID): Result<Passage> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: props.floor1, argumentName: 'floor1' },
            { argument: props.floor2, argumentName: 'floor2' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message)
        } else if (invalidFloors(props.floor1, props.floor2)) {
            return Result.fail('Passage between same building/floors not allowed')
        }

        return Result.ok(new Passage({ ...props }, id))
    }

    update(props: Partial<PassageProps>): Result<Passage> {
        const newF1 = props.floor1 ?? this.props.floor1
        const newF2 = props.floor2 ?? this.props.floor2

        if (invalidFloors(newF1, newF2)) {
            return Result.fail('Passage between same building/floors not allowed')
        }

        this.props.floor1 = newF1
        this.props.floor2 = newF2

        return Result.ok(this)
    }
}
