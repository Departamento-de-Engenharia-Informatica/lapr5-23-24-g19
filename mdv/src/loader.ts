import * as THREE from 'three'

export interface Loader {
    load<T>(url: string): Promise<T>
}

export class NodeLoader implements Loader {
    private cache: { [url in string]: unknown } = {}

    async load<T>(url: string): Promise<T> {
        if (!!this.cache[url]) {
            return Promise.resolve(this.cache[url] as T)
        }

        return await fetch(url)
            .then(async (res) => {
                const json = await res.json()

                this.cache[url] = json
                return json
            })
            .catch((err) => {
                const errmsg = `Error fetching @ ${url}: ${err}`
                console.error(errmsg)
                return Promise.reject(errmsg)
            })
    }
}

export class ThreeLoader implements Loader {
    private loader: THREE.FileLoader

    constructor() {
        THREE.Cache.enabled = true
        this.loader = new THREE.FileLoader().setResponseType('json')
    }

    async load<T>(url: string): Promise<T> {
        return this.loader.loadAsync(url) as T
    }
}
