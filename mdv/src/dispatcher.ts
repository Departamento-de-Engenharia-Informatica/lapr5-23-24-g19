import { EventDispatcher } from 'three'

type DispatcherEvent = 'change-map' | 'enter-elevator' | string

export default class Dispatcher {
    private static _inst = new EventDispatcher()

    public static subscribe(event: DispatcherEvent, handler: Function) {
        Dispatcher._inst.addEventListener(event, (ev) => {
            handler(...ev.message)
        })
    }

    public static emit(event: DispatcherEvent, ...args: any[]) {
        Dispatcher._inst.dispatchEvent({ type: event, message: args })
    }

    static oneShot(event: DispatcherEvent, handler: Function) {
        const wrapper = (...args: any[]) => {
            handler(...args)
            Dispatcher._inst.removeEventListener(event, wrapper)
        }

        Dispatcher._inst.addEventListener(event, wrapper)
    }
}
