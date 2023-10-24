import { AggregateRoot } from '../../core/domain/AggregateRoot'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

import { Result } from '../../core/logic/Result'
import { Guard } from '../../core/logic/Guard'

import {Floor} from '../floor/floor'

interface PassageProps {
    floor1: Floor
    floor2: Floor
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
        if(props.floor1.sameBuilding(props.floor2)){
            return Result.fail<Passage>("Passage between same building not allowed")   
        }
        if (!guardResult.succeeded) {
            return Result.fail<Passage>(guardResult.message)
        }else{
            return Result.ok<Passage>(new Passage({ ...props }, id))

        }
    }
}
