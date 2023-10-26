import { ValueObject } from '../../core/domain/ValueObject'

enum State {
    ENABLED,
    DISABLED,
}

interface Props {
    value: State
}

export class RobotState extends ValueObject<Props> {
    private constructor(props: Props) {
        super(props)
    }

    static create(state?: State): RobotState {
        return new RobotState({ value: state ?? State.ENABLED })
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

    toString(): string {
        switch (this.value) {
            case State.ENABLED:
                return 'Enabled'
            case State.DISABLED:
                return 'Disabled'
            default:
                return 'Unknown'
        }
    }
}
