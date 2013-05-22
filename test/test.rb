
class TestClass
    def self.testfunc(val)
        return val
        case val
            when Hash
                puts "hash"
                val.each do |k,v|
                    puts "    #{k} => #{v}"
                end
                puts "done"
            when Array
                puts "array"
                val.each do |x|
                    puts "    #{x}"
                end
            else
                puts val
        end
        val
    end
end
