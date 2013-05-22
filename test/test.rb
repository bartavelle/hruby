
class TestClass
    def self.testfunc(val)
        case val
            when Hash
                puts "hash"
                val.each do |k,v|
                    puts "#{k} =>"
                    self.testfunc(v)
                end
            when Array
                puts "array"
                val.each do |x|
                    self.testfunc(x)
                end
            else
                puts val
        end
        val
    end
end
