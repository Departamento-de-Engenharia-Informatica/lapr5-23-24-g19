import { Guard } from '../../core/logic/Guard'
import { Result } from '../../core/logic/Result'
import { ValueObject } from '../../core/domain/ValueObject'

interface Props {
    value: string
}

export class TaskType extends ValueObject<Props> {
    private constructor(props: Props) {
        super(props)
    }

    public static create(dto: Props): Result<TaskType> {
        const guardResult = Guard.againstNullOrUndefined(dto.value, 'name')
        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message ?? '')
        }

        return Result.ok(new TaskType({ value: dto.value.trim() }))
    }

    get value(): string {
        return this.props.value
    }
}
