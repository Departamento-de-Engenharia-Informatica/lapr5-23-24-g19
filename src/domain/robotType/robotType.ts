import { AggregateRoot } from '../../core/domain/AggregateRoot'
import { Guard } from '../../core/logic/Guard'
import { Result } from '../../core/logic/Result'
import { UniqueEntityID } from '../../core/domain/UniqueEntityID'

import { RobotTypeCode as Code } from './robotTypeCode'
import { RobotTypeBrand as Brand } from './robotTypeBrand'
import { RobotTypeModel as Model } from './robotTypeModel'
import { TaskType } from './taskType'

interface Props {
    code: Code
    brand: Brand
    model: Model
    taskType: TaskType[]
}

export default class RobotType extends AggregateRoot<Props> {
    private constructor(props: Props, id?: UniqueEntityID) {
        super(props, id)
    }

    public static create(props: Props, id?: UniqueEntityID): Result<RobotType> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: props.code, argumentName: 'robotTypeCode' },
            { argument: props.brand, argumentName: 'robotTypeBrand' },
            { argument: props.model, argumentName: 'robotTypeModel' },
            { argument: props.taskType, argumentName: 'robotTypeTaskType' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message ?? '')
        }

        return Result.ok(new RobotType({ ...props }, id))
    }

    get id(): UniqueEntityID {
        return this._id
    }

    get code(): Code {
        return this.props.code
    }

    get brand(): Brand {
        return this.props.brand
    }

    get model(): Model {
        return this.props.model
    }

    get taskType(): TaskType[] {
        return this.props.taskType
    }
}
