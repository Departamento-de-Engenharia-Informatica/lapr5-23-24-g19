import { ValueObject } from "../../core/domain/ValueObject";
import { Result } from "../../core/logic/Result";

enum State {
    ENABLED,
    DISABLED
}

interface Props {
    value: State
}

export class RobotState extends ValueObject<Props> {
    private constructor(props: Props) {
        super(props)
    }

    static create(): Result<RobotState> {
        return Result.ok(new RobotState({ value: State.ENABLED }))
    }

    set value(state: State) {
        this.props.value = state
    }

    get value(): State {
        return this.props.value
    }

    disable() {
        this.value = State.DISABLED
    }

    private enable() {
        this.value = State.ENABLED
    }
}
