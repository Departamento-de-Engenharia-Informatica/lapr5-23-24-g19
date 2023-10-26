import { AggregateRoot } from "../../core/domain/AggregateRoot";
import { UniqueEntityID } from "../../core/domain/UniqueEntityID";
import { Guard } from "../../core/logic/Guard";
import { Result } from "../../core/logic/Result";
import Type from "../robotType/robotType";
import { RobotCode as Code } from "./code";
import { RobotDescription as Description } from "./description";
import { RobotNickname as Nickname } from "./nickname";
import { RobotSerialNumber as SerialNumber } from "./serialNumber";
import { RobotState as State } from "./state";

interface Props {
    code: Code
    nickname: Nickname
    type: Type
    serialNumber: SerialNumber
    state: State
    description?: Description
}

export default class Robot extends AggregateRoot<Props> {
    private constructor(props: Props, id?: UniqueEntityID) {
        super(props, id)
    }

    public static create(props: Props, id?: UniqueEntityID): Result<Robot> {
        const guardResult = Guard.againstNullOrUndefinedBulk([
            { argument: props.code, argumentName: 'robotCode' },
            { argument: props.nickname, argumentName: 'robotNickname' },
            { argument: props.type, argumentName: 'robotType' },
            { argument: props.serialNumber, argumentName: 'robotSerialNumber' },
            { argument: props.state, argumentName: 'robotState' },
        ])

        if (!guardResult.succeeded) {
            return Result.fail(guardResult.message ?? '')
        }

        return Result.ok(new Robot({ ...props }, id))
    }

    get id(): UniqueEntityID {
        return this._id
    }

    get code(): Code {
        return this.props.code
    }

    get nickname(): Nickname {
        return this.props.nickname
    }

    get type(): Type {
        return this.props.type
    }

    get serialNumber(): SerialNumber {
        return this.props.serialNumber
    }

    get state(): State {
        return this.props.state
    }

    get description(): Description | undefined {
        return this.props.description
    }
}
